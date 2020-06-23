{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.AWS
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

import qualified Brig.Options as Opt
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import qualified Control.Monad.Trans.AWS as AWST
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Aeson hiding ((.=))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.UUID hiding (null)
import Data.Yaml (FromJSON (..))
import Imports hiding (group)
import Network.AWS (AWSRequest, Rs)
import qualified Network.AWS as AWS
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.DynamoDB as DDB
import qualified Network.AWS.Env as AWS
import qualified Network.AWS.SES as SES
import Network.AWS.SQS (rmrsMessages)
import qualified Network.AWS.SQS as SQS
import Network.AWS.SQS.Types hiding (sqs)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import Network.HTTP.Types.Status (status400)
import Network.Mail.Mime
import qualified System.Logger as Logger
import System.Logger.Class
import UnliftIO.Async
import UnliftIO.Exception
import Util.Options

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
      MonadResource
    )

instance MonadUnliftIO Amazon where
  askUnliftIO = Amazon . ReaderT $ \r ->
    withUnliftIO $ \u ->
      return (UnliftIO (unliftIO u . flip runReaderT r . unAmazon))

instance MonadLogger Amazon where
  log l m = view logger >>= \g -> Logger.log g l m

instance AWS.MonadAWS Amazon where
  liftAWS a = view amazonkaEnv >>= flip AWS.runAWS a

mkEnv :: Logger -> Opt.AWSOpts -> Maybe Opt.EmailAWSOpts -> Manager -> IO Env
mkEnv lgr opts emailOpts mgr = do
  let g = Logger.clone (Just "aws.brig") lgr
  let pk = Opt.prekeyTable opts
  let sesEndpoint = mkEndpoint SES.ses . Opt.sesEndpoint <$> emailOpts
  e <-
    mkAwsEnv
      g
      sesEndpoint
      (mkEndpoint SQS.sqs (Opt.sqsEndpoint opts))
      (mkEndpoint DDB.dynamoDB (Opt.dynamoDBEndpoint opts))
  sq <- maybe (return Nothing) (fmap Just . getQueueUrl e . Opt.sesQueue) emailOpts
  jq <- maybe (return Nothing) (fmap Just . getQueueUrl e) (Opt.userJournalQueue opts)
  return (Env g sq jq pk e)
  where
    mkEndpoint svc e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) svc
    mkAwsEnv g ses sqs dyn =
      set AWS.envLogger (awsLogger g)
        <$> AWS.newEnvWith AWS.Discover Nothing mgr
        <&> maybe id AWS.configure ses
        <&> AWS.configure sqs
        <&> AWS.configure dyn
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
getQueueUrl e q = view SQS.gqursQueueURL <$> exec e (SQS.getQueueURL q)

execute :: MonadIO m => Env -> Amazon a -> m a
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
  msgs <- view rmrsMessages <$> send receive
  void $ mapConcurrently onMessage msgs
  when (null msgs) $
    threadDelay (1000 * throttleMillis)
  where
    receive =
      SQS.receiveMessage url
        & set SQS.rmWaitTimeSeconds (Just 20)
          . set SQS.rmMaxNumberOfMessages (Just 10)
    onMessage m =
      case decodeStrict =<< Text.encodeUtf8 <$> m ^. mBody of
        Nothing -> err $ msg ("Failed to parse SQS event: " ++ show m)
        Just n -> do
          debug $ msg ("Received SQS event: " ++ show n)
          liftIO $ callback n
          for_ (m ^. mReceiptHandle) (void . send . SQS.deleteMessage url)
    unexpectedError x = do
      err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
      threadDelay 3000000

enqueueStandard :: Text -> BL.ByteString -> Amazon SQS.SendMessageResponse
enqueueStandard url m = retrying retry5x (const canRetry) (const (sendCatch req)) >>= throwA
  where
    req = SQS.sendMessage url $ Text.decodeLatin1 (BL.toStrict m)

enqueueFIFO :: Text -> Text -> UUID -> BL.ByteString -> Amazon SQS.SendMessageResponse
enqueueFIFO url group dedup m = retrying retry5x (const canRetry) (const (sendCatch req)) >>= throwA
  where
    req =
      SQS.sendMessage url (Text.decodeLatin1 (BL.toStrict m))
        & SQS.smMessageGroupId .~ Just group
        & SQS.smMessageDeduplicationId .~ Just (toText dedup)

-------------------------------------------------------------------------------
-- SES

sendMail :: Mail -> Amazon ()
sendMail m = do
  body <- liftIO $ BL.toStrict <$> renderMail' m
  let raw =
        SES.sendRawEmail (SES.rawMessage body)
          & SES.sreDestinations .~ fmap addressEmail (mailTo m)
          & SES.sreSource ?~ addressEmail (mailFrom m)
  resp <- retrying retry5x (const canRetry) $ const (sendCatch raw)
  void $ either check return resp
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
        | se ^. AWS.serviceStatus == status400
            && "Invalid domain name" `Text.isPrefixOf` AWS.toText (se ^. AWS.serviceCode) ->
          throwM SESInvalidDomain
      _ -> throwM (GeneralError x)

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = throwA =<< sendCatch r

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) return

execCatch ::
  (AWSRequest a, AWS.HasEnv r, MonadUnliftIO m, MonadCatch m, MonadThrow m, MonadIO m) =>
  r ->
  a ->
  m (Either AWS.Error (Rs a))
execCatch e cmd =
  runResourceT . AWST.runAWST e $
    AWST.trying AWS._Error $
      AWST.send cmd

exec ::
  (AWSRequest a, AWS.HasEnv r, MonadUnliftIO m, MonadCatch m, MonadThrow m, MonadIO m) =>
  r ->
  a ->
  m (Rs a)
exec e cmd = execCatch e cmd >>= either (throwM . GeneralError) return

canRetry :: MonadIO m => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
  AWS.ServiceError se | se ^. AWS.serviceCode == AWS.ErrorCode "RequestThrottled" -> pure True
  _ -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
