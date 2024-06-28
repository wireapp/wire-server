{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.AWS
  ( -- * Monad
    Env,
    mkEnv,
    Amazon,
    amazonkaEnv,
    execute,
    sesQueue,
    userJournalQueue,
    prekeyTable,
    Error (..),

    -- * SES
    sendMail,

    -- * SQS
    listen,
    enqueueFIFO,
    enqueueStandard,
    getQueueUrl,

    -- * AWS
    exec,
    execCatch,
  )
where

import Amazonka (AWSRequest, AWSResponse)
import Amazonka qualified as AWS
import Amazonka.Data.Text qualified as AWS
import Amazonka.DynamoDB qualified as DDB
import Amazonka.SES qualified as SES
import Amazonka.SES.Lens qualified as SES
import Amazonka.SQS qualified as SQS
import Amazonka.SQS.Lens qualified as SQS
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Aeson hiding ((.=))
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.UUID hiding (null)
import Imports hiding (group)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import Network.HTTP.Types.Status (status400)
import Network.Mail.Mime
import System.Logger qualified as Logger
import System.Logger.Class
import UnliftIO.Async
import UnliftIO.Exception
import Util.Options
import Wire.API.AWS.Options qualified as Opt

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
  deriving
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

mkEnv :: Logger -> Opt.AWSOpts -> Maybe Opt.EmailAWSOpts -> Manager -> IO Env
mkEnv lgr opts emailOpts mgr = do
  let g = Logger.clone (Just "aws.brig") lgr
  let pk = Opt.prekeyTable opts
  let sesEndpoint = mkEndpoint SES.defaultService . Opt.sesEndpoint <$> emailOpts
  let dynamoEndpoint = mkEndpoint DDB.defaultService <$> Opt.dynamoDBEndpoint opts
  e <-
    mkAwsEnv
      g
      sesEndpoint
      dynamoEndpoint
      (mkEndpoint SQS.defaultService (Opt.sqsEndpoint opts))
  sq <- maybe (pure Nothing) (fmap Just . getQueueUrl e . Opt.sesQueue) emailOpts
  jq <- maybe (pure Nothing) (fmap Just . getQueueUrl e) (Opt.userJournalQueue opts)
  pure (Env g sq jq pk e)
  where
    mkEndpoint svc e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) svc
    mkAwsEnv g ses dyn sqs = do
      baseEnv <-
        AWS.newEnv AWS.discover
          <&> AWS.configureService sqs . maybe id AWS.configureService dyn . maybe id AWS.configureService ses
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

getQueueUrl ::
  (MonadUnliftIO m, MonadCatch m) =>
  AWS.Env ->
  Text ->
  m Text
getQueueUrl e q = view SQS.getQueueUrlResponse_queueUrl <$> exec e (SQS.newGetQueueUrl q)

execute :: (MonadIO m) => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

data Error where
  GeneralError :: (Show e, AWS.AsError e) => e -> Error
  SESInvalidDomain :: Error

deriving instance Show Error

deriving instance Typeable Error

instance Exception Error

--------------------------------------------------------------------------------
-- SQS

listen :: (FromJSON a, Show a) => Int -> Text -> (a -> IO ()) -> Amazon ()
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
      case decodeStrict . Text.encodeUtf8 =<< (m ^. SQS.message_body) of
        Nothing -> err $ msg ("Failed to parse SQS event: " ++ show m)
        Just n -> do
          debug $ msg ("Received SQS event: " ++ show n)
          liftIO $ callback n
          for_ (m ^. SQS.message_receiptHandle) (void . send . SQS.newDeleteMessage url)
    unexpectedError x = do
      err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
      threadDelay 3000000

enqueueStandard :: Text -> BL.ByteString -> Amazon SQS.SendMessageResponse
enqueueStandard url m = retrying retry5x (const canRetry) (const (sendCatch req)) >>= throwA
  where
    req = SQS.newSendMessage url $ Text.decodeLatin1 (BL.toStrict m)

enqueueFIFO :: Text -> Text -> UUID -> BL.ByteString -> Amazon SQS.SendMessageResponse
enqueueFIFO url group dedup m = retrying retry5x (const canRetry) (const (sendCatch req)) >>= throwA
  where
    req =
      SQS.newSendMessage url (Text.decodeLatin1 (BL.toStrict m))
        & SQS.sendMessage_messageGroupId ?~ group
        & SQS.sendMessage_messageDeduplicationId ?~ toText dedup

-------------------------------------------------------------------------------
-- SES

sendMail :: Mail -> Amazon ()
sendMail m = do
  body <- liftIO $ BL.toStrict <$> renderMail' m
  let raw =
        SES.newSendRawEmail (SES.newRawMessage body)
          & SES.sendRawEmail_destinations ?~ fmap addressEmail (mailTo m)
          & SES.sendRawEmail_source ?~ addressEmail (mailFrom m)
  resp <- retrying retry5x (const canRetry) $ const (sendCatch raw)
  void $ either check pure resp
  where
    check x = case x of
      -- To map rejected domain names by SES to 400 responses, in order
      -- not to trigger false 5xx alerts. Upfront domain name validation
      -- is only according to the syntax rules of RFC5322 but additional
      -- constraints may be applied by email servers (in this case SES).
      -- Since such additional constraints are neither standardised nor
      -- documented in the cases of SES, we can only handle the errors
      -- after the fact.
      AWS.ServiceError se
        | se
            ^. AWS.serviceError_status
            == status400
            && "Invalid domain name"
              `Text.isPrefixOf` AWS.toText (se ^. AWS.serviceError_code) ->
            throwM SESInvalidDomain
      _ -> throwM (GeneralError x)

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: (AWSRequest r, Typeable r, Typeable (AWSResponse r)) => r -> Amazon (Either AWS.Error (AWSResponse r))
sendCatch req = do
  env <- view amazonkaEnv
  AWS.trying AWS._Error . AWS.send env $ req

send ::
  (AWSRequest r, Typeable r, Typeable (AWSResponse r)) =>
  r ->
  Amazon (AWSResponse r)
send r = throwA =<< sendCatch r

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) pure

execCatch ::
  ( AWSRequest a,
    Typeable a,
    MonadUnliftIO m,
    Typeable (AWSResponse a),
    MonadCatch m
  ) =>
  AWS.Env ->
  a ->
  m (Either AWS.Error (AWSResponse a))
execCatch e cmd =
  runResourceT $
    AWS.trying AWS._Error $
      AWS.send e cmd

exec ::
  ( AWSRequest a,
    Typeable a,
    Typeable (AWSResponse a),
    MonadCatch m,
    MonadIO m
  ) =>
  AWS.Env ->
  a ->
  m (AWSResponse a)
exec e cmd = liftIO (execCatch e cmd) >>= either (throwM . GeneralError) pure

canRetry :: (MonadIO m) => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
  AWS.ServiceError se | se ^. AWS.serviceError_code == AWS.ErrorCode "RequestThrottled" -> pure True
  _ -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
