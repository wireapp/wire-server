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

module Galley.Aws
  ( Env,
    mkEnv,
    awsEnv,
    eventQueue,
    QueueUrl (..),
    Amazon,
    execute,
    enqueue,

    -- * Errors
    Error (..),
  )
where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import qualified Control.Monad.Trans.AWS as AWST
import Control.Monad.Trans.Resource
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder (toLazyByteString)
import Data.ProtoLens.Encoding
import Data.Text.Encoding (decodeLatin1)
import Data.UUID (toText)
import Data.UUID.V4
import Galley.Options
import Imports
import qualified Network.AWS as AWS
import qualified Network.AWS.Env as AWS
import qualified Network.AWS.SQS as SQS
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    Manager,
  )
import qualified Network.TLS as TLS
import qualified Proto.TeamEvents as E
import qualified System.Logger as Logger
import System.Logger.Class
import Util.Options

newtype QueueUrl = QueueUrl Text
  deriving (Show)

data Error where
  GeneralError :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show Error

deriving instance Typeable Error

instance Exception Error

data Env = Env
  { _awsEnv :: !AWS.Env,
    _logger :: !Logger,
    _eventQueue :: !QueueUrl
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
  liftAWS aws = view awsEnv >>= \e -> AWS.runAWS e aws

mkEnv :: Logger -> Manager -> JournalOpts -> IO Env
mkEnv lgr mgr opts = do
  let g = Logger.clone (Just "aws.galley") lgr
  e <- mkAwsEnv g
  q <- getQueueUrl e (opts ^. awsQueueName)
  return (Env e g q)
  where
    sqs e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) SQS.sqs
    mkAwsEnv g =
      set AWS.envLogger (awsLogger g)
        . set AWS.envRetryCheck retryCheck
        <$> AWS.newEnvWith AWS.Discover Nothing mgr
        <&> AWS.configure (sqs (opts ^. awsEndpoint))
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
    -- TODO: Remove custom retryCheck? Should be fixed since tls 1.3.9?
    -- account occasional TLS handshake failures.
    -- See: https://github.com/vincenthz/hs-tls/issues/124
    -- See: https://github.com/brendanhay/amazonka/issues/269
    retryCheck _ InvalidUrlException {} = False
    retryCheck n (HttpExceptionRequest _ ex) = case ex of
      _ | n >= 3 -> False
      NoResponseDataReceived -> True
      ConnectionTimeout -> True
      ConnectionClosed -> True
      ConnectionFailure _ -> True
      InternalException x -> case fromException x of
        Just TLS.HandshakeFailed {} -> True
        _ -> False
      _ -> False
    getQueueUrl :: AWS.Env -> Text -> IO QueueUrl
    getQueueUrl e q = do
      x <-
        runResourceT . AWST.runAWST e $
          AWST.trying AWS._Error $
            AWST.send (SQS.getQueueURL q)
      either
        (throwM . GeneralError)
        (return . QueueUrl . view SQS.gqursQueueURL)
        x

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

enqueue :: E.TeamEvent -> Amazon ()
enqueue e = do
  QueueUrl url <- view eventQueue
  rnd <- liftIO nextRandom
  res <- retrying (limitRetries 5 <> exponentialBackoff 1000000) (const canRetry) $ const (sendCatch (req url rnd))
  either (throwM . GeneralError) (const (return ())) res
  where
    event = decodeLatin1 $ B64.encode $ encodeMessage e
    req url dedup =
      SQS.sendMessage url event & SQS.smMessageGroupId .~ Just "team.events"
        & SQS.smMessageDeduplicationId .~ Just (toText dedup)

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWS.AWSRequest r => r -> Amazon (Either AWS.Error (AWS.Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

canRetry :: MonadIO m => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
  AWS.ServiceError se | se ^. AWS.serviceCode == AWS.ErrorCode "RequestThrottled" -> pure True
  _ -> pure False
