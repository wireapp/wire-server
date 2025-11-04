{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.AWSSubsystem.AWS where

import Amazonka (AWSRequest, AWSResponse)
import Amazonka qualified as AWS
import Amazonka.DynamoDB qualified as DDB
import Amazonka.SES qualified as SES
import Amazonka.SQS qualified as SQS
import Amazonka.SQS.Lens qualified as SQS
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Aeson hiding ((.=))
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as Text
import Data.UUID hiding (null)
import Imports hiding (group)
import Network.HTTP.Client (Manager)
import Polysemy hiding (send)
import Polysemy.Final
import Polysemy.Input (runInputConst)
import System.Logger qualified as Logger
import System.Logger.Class
import UnliftIO.Async
import UnliftIO.Exception
import Util.Options
import Wire.AWS
import Wire.AWSSubsystem (AWSSubsystem (..), AWSSubsystemError (..))

data Env = Env
  { _logger :: !Logger,
    _sesQueue :: !(Maybe Text),
    _userJournalQueue :: !(Maybe Text),
    _prekeyTable :: !Text,
    _amazonkaEnv :: !AWS.Env
  }

makeLenses ''Env

newtype Amazon a = Amazon
  { unAmazon :: ReaderT Env (ResourceT IO) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadResource,
      MonadUnliftIO
    )

instance MonadLogger Amazon where
  log l m = view logger >>= \g -> Logger.log g l m

data AWSOpts = AWSOpts
  { -- | Event journal queue for user events
    --   (e.g. user deletion)
    userJournalQueue :: !(Maybe Text),
    -- | Dynamo table for storing prekey data
    prekeyTable :: !Text,
    -- | AWS SQS endpoint
    sqsEndpoint :: !AWSEndpoint,
    -- | DynamoDB endpoint
    dynamoDBEndpoint :: !(Maybe AWSEndpoint)
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data EmailAWSOpts = EmailAWSOpts
  { -- | Event feedback queue for SES
    --   (e.g. for email bounces and complaints)
    sesQueue :: !Text,
    -- | AWS SES endpoint
    sesEndpoint :: !AWSEndpoint
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

mkEnv :: Logger -> AWSOpts -> Maybe EmailAWSOpts -> Manager -> IO Env
mkEnv lgr opts emailOpts mgr = do
  let g = Logger.clone (Just "aws.brig") lgr
  let pk = opts.prekeyTable
  let sesEndpoint = mkEndpoint SES.defaultService . (.sesEndpoint) <$> emailOpts
  let dynamoEndpoint = mkEndpoint DDB.defaultService <$> opts.dynamoDBEndpoint
  e <-
    mkAwsEnv
      g
      sesEndpoint
      dynamoEndpoint
      (mkEndpoint SQS.defaultService opts.sqsEndpoint)
  sq <- maybe (pure Nothing) (fmap Just . getQueueUrl e . (.sesQueue)) emailOpts
  jq <- maybe (pure Nothing) (fmap Just . getQueueUrl e) opts.userJournalQueue
  pure (Env g sq jq pk e)
  where
    mkEndpoint svc e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) svc
    mkAwsEnv g ses dyn sqs = do
      baseEnv <-
        AWS.newEnv AWS.discover
          <&> AWS.configureService sqs
            . maybe id AWS.configureService dyn
            . maybe id AWS.configureService ses
      pure $
        baseEnv
          { AWS.logger = awsLogger g,
            AWS.manager = mgr
          }
    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString
    mapLevel AWS.Info = Logger.Info
    -- Debug output from amazonka can be very useful for tracing requests
    -- but is very verbose (and multiline which we don't handle well)
    -- distracting from our own debug logs, so we map amazonka's 'Debug'
    -- level to our 'Trace' level.
    mapLevel AWS.Debug = Logger.Trace
    mapLevel AWS.Trace = Logger.Trace
    -- n.b. Errors are either returned or thrown. In both cases they will
    -- already be logged if left unhandled. We don't want errors to be
    -- logged inside amazonka already, before we even had a chance to handle
    -- them, which results in distracting noise. For debugging purposes,
    -- they are still revealed on debug level.
    mapLevel AWS.Error = Logger.Debug

---------------------------------------------------------

-- | Variant of getQueueUrlImpl for calling during Env construction.
getQueueUrl ::
  (MonadUnliftIO m, MonadCatch m) =>
  AWS.Env ->
  Text ->
  m Text
getQueueUrl e q = view SQS.getQueueUrlResponse_queueUrl <$> runAwsRequestThrow e (SQS.newGetQueueUrl q)

getQueueUrlImpl :: Env -> Text -> IO Text
getQueueUrlImpl env queueName = do
  resp <- runResourceT $ AWS.send env._amazonkaEnv (SQS.newGetQueueUrl queueName)
  pure $ view SQS.getQueueUrlResponse_queueUrl resp

getJournalQueueUrlImpl :: Env -> IO (Maybe Text)
getJournalQueueUrlImpl env = pure (env ^. userJournalQueue)

listen :: (FromJSON a, Show a) => Int -> Text -> (a -> IO x) -> Amazon y
listen throttleMillis url callback = forever . handleAny unexpectedError $ do
  msgs <- fromMaybe [] . view SQS.receiveMessageResponse_messages <$> send receive
  void $ mapConcurrently onMessage msgs
  when (null msgs) $
    threadDelay (1000 * throttleMillis)
  where
    receive =
      SQS.newReceiveMessage url
        & set SQS.receiveMessage_waitTimeSeconds (Just 20)
          . set SQS.receiveMessage_maxNumberOfMessages (Just 10)
    onMessage m =
      case eitherDecodeStrict . Text.encodeUtf8 =<< maybe (Left "No message body received") Right (m ^. SQS.message_body) of
        Left e -> err $ msg (val "Failed to parse SQS event") . field "error" e . field "message" (show m)
        Right n -> do
          debug $ msg (val "Received SQS event") . field "event" (show n)
          liftIO $ void $ callback n
          for_ (m ^. SQS.message_receiptHandle) (void . send . SQS.newDeleteMessage url)
    unexpectedError x = do
      err $ "error" .= show x ~~ msg (val "Failed to read or process message from SQS")
      threadDelay 3000000

enqueueStandard :: Text -> BL.ByteString -> Amazon SQS.SendMessageResponse
enqueueStandard url m = retrying retry5x (const $ pure . canRetry) (const (sendCatchAmazon req)) >>= throwA
  where
    req = SQS.newSendMessage url $ Text.decodeLatin1 (BL.toStrict m)

enqueueFIFO :: Text -> Text -> UUID -> BL.ByteString -> Amazon SQS.SendMessageResponse
enqueueFIFO url group dedup m = retrying retry5x (const $ pure . canRetry) (const (sendCatchAmazon req)) >>= throwA
  where
    req =
      SQS.newSendMessage url (Text.decodeLatin1 (BL.toStrict m))
        & SQS.sendMessage_messageGroupId ?~ group
        & SQS.sendMessage_messageDeduplicationId ?~ toText dedup

--------------------------------------------------------------------------------
-- Utilities

send ::
  (AWSRequest r, Typeable r, Typeable (AWSResponse r)) =>
  r ->
  Amazon (AWSResponse r)
send r = throwA =<< sendCatchAmazon r

-- | Temporary helper to translate polysemy to Amazon monad, it should go away
-- with more polysemisation
sendCatchAmazon :: (AWSRequest req, Typeable req, Typeable (AWSResponse req)) => req -> Amazon (Either AWS.Error (AWS.AWSResponse req))
sendCatchAmazon req = do
  env <- view amazonkaEnv
  liftIO . runM . runInputConst env $ sendCatch req

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) pure

runAwsRequest ::
  ( AWSRequest a,
    Typeable a,
    MonadUnliftIO m,
    Typeable (AWSResponse a),
    MonadCatch m
  ) =>
  AWS.Env ->
  a ->
  m (Either AWS.Error (AWSResponse a))
runAwsRequest e cmd =
  runResourceT $
    AWS.trying AWS._Error $
      AWS.send e cmd

runAwsRequestThrow ::
  ( AWSRequest a,
    Typeable a,
    Typeable (AWSResponse a),
    MonadCatch m,
    MonadIO m
  ) =>
  AWS.Env ->
  a ->
  m (AWSResponse a)
runAwsRequestThrow e cmd = liftIO (runAwsRequest e cmd) >>= either (throwM . GeneralError) pure

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000

--------------------------------------------------------------------------------
-- Polysemy Interpreter

-- | Run AWSSubsystem effect by interpreting it into the Amazon monad.
-- Uses Final IO strategy for the higher-order Listen effect.
runAWSSubsystem ::
  (Member (Final IO) r) =>
  Env ->
  Sem (AWSSubsystem : r) a ->
  Sem r a
runAWSSubsystem env = interpretFinal $ \case
  RunAwsRequest x -> liftS @IO $ runAwsRequest env._amazonkaEnv x
  RunAwsRequestThrow x -> liftS @IO $ runAwsRequestThrow env._amazonkaEnv x
  GetQueueUrl queueName -> liftS @IO $ getQueueUrlImpl env queueName
  GetJournalQueueUrl -> liftS @IO $ getJournalQueueUrlImpl env
  EnqueueStandard url message -> liftS $ do
    runResourceT $ runReaderT ((enqueueStandard url message).unAmazon) env
  EnqueueFIFO url group dedupId message -> liftS $ do
    runResourceT $ runReaderT ((enqueueFIFO url group dedupId message).unAmazon) env
  Listen throttle url callback -> do
    callbackS <- bindS callback
    s <- getInitialStateS
    pure $ runResourceT $ runReaderT ((listen throttle url $ callbackS . (s $>)).unAmazon) env
