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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.AWS where

import Amazonka qualified as AWS
import Amazonka.SQS qualified as SQS
import Amazonka.SQS.Lens qualified as SQS
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Builder (toLazyByteString)
import Data.ProtoLens.Encoding (encodeMessage)
import Data.Text.Encoding (decodeLatin1)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Imports
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    Manager,
  )
import Network.TLS qualified as TLS
import Polysemy (Embed, Member, Sem, embed)
import Polysemy.Input (Input, input)
import Proto.TeamEvents qualified as E
import System.Logger qualified as Logger
import System.Logger.Class (Logger, MonadLogger (..))
import Util.Options (AWSEndpoint (..), awsHost, awsPort, awsSecure)

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
      MonadResource,
      MonadUnliftIO
    )

instance MonadLogger Amazon where
  log l m = view logger >>= \g -> Logger.log g l m

mkEnv :: Logger -> Manager -> AWSEndpoint -> Text -> IO Env
mkEnv lgr mgr endpoint qname = do
  let g = Logger.clone (Just "aws") lgr
  e <- mkAwsEnv g
  q <- getQueueUrl e qname
  pure (Env e g (QueueUrl q))
  where
    sqs e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) SQS.defaultService
    mkAwsEnv g = do
      baseEnv <-
        AWS.newEnv AWS.discover
          <&> AWS.configureService (sqs endpoint)
      pure $
        baseEnv
          { AWS.logger = awsLogger g,
            AWS.retryCheck = retryCheck,
            AWS.manager = mgr
          }
    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString
    mapLevel AWS.Info = Logger.Info
    mapLevel AWS.Debug = Logger.Trace
    mapLevel AWS.Trace = Logger.Trace
    mapLevel AWS.Error = Logger.Debug
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
    getQueueUrl :: AWS.Env -> Text -> IO Text
    getQueueUrl e q = do
      x <-
        runResourceT $
          AWS.trying AWS._Error $
            AWS.send e (SQS.newGetQueueUrl q)
      either
        (throwM . GeneralError)
        (pure . view SQS.getQueueUrlResponse_queueUrl)
        x

execute :: (MonadIO m) => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

enqueue :: E.TeamEvent -> Amazon ()
enqueue ev = do
  QueueUrl url <- view eventQueue
  dedup <- liftIO nextRandom
  amaznkaEnv <- view awsEnv
  let body = decodeLatin1 $ B64.encode $ encodeMessage ev
      req =
        SQS.newSendMessage url body
          & SQS.sendMessage_messageGroupId ?~ "team.events"
          & SQS.sendMessage_messageDeduplicationId ?~ toText dedup
  res <- retrying (limitRetries 5 <> exponentialBackoff 1000000) (const (pure . canRetry)) $ const (sendCatchEnv amaznkaEnv req)
  either (throwM . GeneralError) (const (pure ())) res

-- Polysemy-style helper used by existing code
sendCatch ::
  ( Member (Embed IO) r,
    Member (Input AWS.Env) r,
    AWS.AWSRequest req,
    Typeable req,
    Typeable (AWS.AWSResponse req)
  ) =>
  req ->
  Sem r (Either AWS.Error (AWS.AWSResponse req))
sendCatch req = do
  env <- input
  embed $ runResourceT (AWS.trying AWS._Error (AWS.send env req))

-- Amazon monad variant
sendCatchEnv ::
  ( AWS.AWSRequest r,
    Typeable r,
    Typeable (AWS.AWSResponse r)
  ) =>
  AWS.Env ->
  r ->
  Amazon (Either AWS.Error (AWS.AWSResponse r))
sendCatchEnv e = AWS.trying AWS._Error . AWS.send e

canRetry :: Either AWS.Error a -> Bool
canRetry (Right _) = False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> True
  AWS.ServiceError se | se ^. AWS.serviceError_code == AWS.ErrorCode "RequestThrottled" -> True
  _ -> False
