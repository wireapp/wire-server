{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module CargoHold.AWS
  ( -- * Monad
    Env (..),
    mkEnv,
    amazonkaEnvWithDownloadEndpoint,
    Amazon,
    execute,
    Error (..),

    -- * AWS
    sendCatch,
    exec,
    execStream,
    execCatch,
  )
where

import Amazonka (AWSRequest, AWSResponse)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import CargoHold.CloudFront
import CargoHold.Options hiding (cloudFront, s3Bucket)
import Conduit
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Builder (toLazyByteString)
import Imports
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import qualified System.Logger as Logger
import System.Logger.Class (Logger, MonadLogger (log), (~~))
import qualified System.Logger.Class as Log
import Util.Options (AWSEndpoint (..))

data Env = Env
  { logger :: !Logger,
    s3Bucket :: !Text,
    amazonkaEnv :: !AWS.Env,
    -- | Endpoint for downloading assets (for the external world).
    -- This gets used with Minio, which Cargohold can reach using a cluster-internal endpoint,
    -- but clients can't, so we need to use a public one for pre-signed URLs we redirect to.
    amazonkaDownloadEndpoint :: !AWSEndpoint,
    cloudFront :: !(Maybe CloudFront)
  }

-- | Override the endpoint in the '_amazonkaEnv' with '_amazonkaDownloadEndpoint'.
-- TODO: Choose the correct s3 addressing style
amazonkaEnvWithDownloadEndpoint :: Env -> AWS.Env
amazonkaEnvWithDownloadEndpoint e =
  AWS.overrideService (setAWSEndpoint e.amazonkaDownloadEndpoint) e.amazonkaEnv

setAWSEndpoint :: AWSEndpoint -> AWS.Service -> AWS.Service
setAWSEndpoint e = AWS.setEndpoint (_awsSecure e) (_awsHost e) (_awsPort e)

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
  log l m = asks (.logger) >>= \g -> Logger.log g l m

mkEnv ::
  Logger ->
  -- | S3 endpoint
  AWSEndpoint ->
  AWS.S3AddressingStyle ->
  -- | Endpoint for downloading assets (for the external world)
  AWSEndpoint ->
  -- | Bucket
  Text ->
  Maybe CloudFrontOpts ->
  Manager ->
  IO Env
mkEnv lgr s3End s3AddrStyle s3Download bucket cfOpts mgr = do
  let g = Logger.clone (Just "aws.cargohold") lgr
  e <- mkAwsEnv g (setAWSEndpoint s3End (S3.defaultService & AWS.service_s3AddressingStyle .~ s3AddrStyle))
  cf <- mkCfEnv cfOpts
  pure (Env g bucket e s3Download cf)
  where
    mkCfEnv (Just o) = Just <$> initCloudFront o.privateKey o.keyPairId 300 o.domain
    mkCfEnv Nothing = pure Nothing
    mkAwsEnv g s3 = do
      baseEnv <-
        AWS.newEnv AWS.discover
          <&> AWS.configureService s3
      pure $
        baseEnv
          { AWS.logger = awsLogger g,
            AWS.manager = mgr
          }
    awsLogger g l = Logger.log g (mapLevel l) . Log.msg . toLazyByteString
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

execute :: (MonadIO m) => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

data Error where
  GeneralError :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show Error

deriving instance Typeable Error

instance Exception Error

--------------------------------------------------------------------------------
-- Utilities

sendCatch ::
  (MonadCatch m, AWSRequest r, MonadResource m, Typeable r, Typeable (AWSResponse r)) =>
  AWS.Env ->
  r ->
  m (Either AWS.Error (AWSResponse r))
sendCatch env = AWS.trying AWS._Error . AWS.send env

exec ::
  ( AWSRequest r,
    Typeable r,
    Typeable (AWSResponse r),
    Show r,
    MonadLogger m,
    MonadIO m,
    MonadThrow m
  ) =>
  Env ->
  (Text -> r) ->
  m (AWSResponse r)
exec env request = do
  let req = request env.s3Bucket
  resp <- execute env (sendCatch env.amazonkaEnv req)
  case resp of
    Left err -> do
      Logger.info env.logger $
        Log.field "remote" (Log.val "S3")
          ~~ Log.msg (show err)
          ~~ Log.msg (show req)
      -- We just re-throw the error, but logging it here also gives us the request
      -- that caused it.
      throwM (GeneralError err)
    Right r -> pure r

execStream ::
  ( AWSRequest r,
    Typeable r,
    Typeable (AWSResponse r),
    Show r
  ) =>
  Env ->
  (Text -> r) ->
  ResourceT IO (AWSResponse r)
execStream env request = do
  let req = request env.s3Bucket
  resp <- sendCatch env.amazonkaEnv req
  case resp of
    Left err -> do
      Logger.info env.logger $
        Log.field "remote" (Log.val "S3")
          ~~ Log.msg (show err)
          ~~ Log.msg (show req)
      -- We just re-throw the error, but logging it here also gives us the request
      -- that caused it.
      throwM (GeneralError err)
    Right r -> pure r

execCatch ::
  ( AWSRequest r,
    Typeable r,
    Typeable (AWSResponse r),
    Show r,
    MonadLogger m,
    MonadIO m
  ) =>
  Env ->
  (Text -> r) ->
  m (Maybe (AWSResponse r))
execCatch env request = do
  let req = request env.s3Bucket
  resp <- execute env (retrying retry5x (const canRetry) (const (sendCatch env.amazonkaEnv req)))
  case resp of
    Left err -> do
      Log.info $
        Log.field "remote" (Log.val "S3")
          ~~ Log.msg (show err)
          ~~ Log.msg (show req)
      pure Nothing
    Right r -> pure $ Just r

canRetry :: (MonadIO m) => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
  AWS.ServiceError se | se ^. AWS.serviceError_code == AWS.ErrorCode "RequestThrottled" -> pure True
  _ -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
