{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

module CargoHold.AWS
  ( -- * Monad
    Env,
    mkEnv,
    useDownloadEndpoint,
    Amazon,
    amazonkaEnv,
    execute,
    s3Bucket,
    cloudFront,
    Error (..),

    -- * AWS
    send,
    sendCatch,
    exec,
    execCatch,
  )
where

import CargoHold.CloudFront
import CargoHold.Options
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import qualified Control.Monad.Trans.AWS as AWST
import Control.Monad.Trans.Resource
import Control.Retry
import Data.ByteString.Builder (toLazyByteString)
import Imports
import Network.AWS (AWSRequest, Rs)
import qualified Network.AWS as AWS
import qualified Network.AWS.Env as AWS
import qualified Network.AWS.S3 as S3
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import qualified System.Logger as Logger
import qualified System.Logger.Class as Log
import System.Logger.Class (Logger, MonadLogger (log), (~~))
import Util.Options (AWSEndpoint (..))

data Env = Env
  { _logger :: !Logger,
    _s3Bucket :: !Text,
    _amazonkaEnv :: !AWS.Env,
    -- | Endpoint for downloading assets (for the external world).
    -- This gets used with Minio, which Cargohold can reach using a cluster-internal endpoint,
    -- but clients can't, so we need to use a public one for pre-signed URLs we redirect to.
    _amazonkaDownloadEndpoint :: !AWSEndpoint,
    _cloudFront :: !(Maybe CloudFront)
  }

makeLenses ''Env

-- | Override the endpoint in the '_amazonkaEnv' with '_amazonkaDownloadEndpoint'.
useDownloadEndpoint :: Env -> Env
useDownloadEndpoint e =
  e & amazonkaEnv %~ AWS.override (setAWSEndpoint (e ^. amazonkaDownloadEndpoint))

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
      MonadResource
    )

instance MonadLogger Amazon where
  log l m = view logger >>= \g -> Logger.log g l m

instance MonadUnliftIO Amazon where
  askUnliftIO = Amazon . ReaderT $ \r ->
    withUnliftIO $ \u ->
      return (UnliftIO (unliftIO u . flip runReaderT r . unAmazon))

instance AWS.MonadAWS Amazon where
  liftAWS a = view amazonkaEnv >>= flip AWS.runAWS a

mkEnv ::
  Logger ->
  -- | S3 endpoint
  AWSEndpoint ->
  -- | Endpoint for downloading assets (for the external world)
  AWSEndpoint ->
  -- | Bucket
  Text ->
  Maybe CloudFrontOpts ->
  Manager ->
  IO Env
mkEnv lgr s3End s3Download bucket cfOpts mgr = do
  let g = Logger.clone (Just "aws.cargohold") lgr
  e <- mkAwsEnv g (setAWSEndpoint s3End S3.s3)
  cf <- mkCfEnv cfOpts
  return (Env g bucket e s3Download cf)
  where
    mkCfEnv (Just o) = Just <$> initCloudFront (o ^. cfPrivateKey) (o ^. cfKeyPairId) 300 (o ^. cfDomain)
    mkCfEnv Nothing = return Nothing
    mkAwsEnv g s3 =
      AWS.newEnvWith AWS.Discover Nothing mgr
        <&> set AWS.envLogger (awsLogger g)
        <&> AWS.configure s3
    awsLogger g l = Logger.log g (mapLevel l) . Log.msg . toLazyByteString
    mapLevel AWS.Info = Logger.Info
    mapLevel AWS.Debug = Logger.Trace
    mapLevel AWS.Trace = Logger.Trace
    mapLevel AWS.Error = Logger.Debug

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

data Error where
  GeneralError :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show Error

deriving instance Typeable Error

instance Exception Error

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = throwA =<< sendCatch r

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) return

exec ::
  (AWSRequest r, MonadIO m) =>
  Env ->
  (Text -> r) ->
  m (Rs r)
exec env request = do
  let bucket = _s3Bucket env
  execute env (AWS.send $ request bucket)

execCatch ::
  (AWSRequest r, Show r, MonadLogger m, MonadIO m) =>
  Env ->
  (Text -> r) ->
  m (Maybe (Rs r))
execCatch env request = do
  let req = request (_s3Bucket env)
  resp <- execute env (retrying retry5x (const canRetry) (const (sendCatch req)))
  case resp of
    Left err -> do
      Log.debug $
        Log.field "remote" (Log.val "S3")
          ~~ Log.msg (show err)
          ~~ Log.msg (show req)
      return Nothing
    Right r -> return $ Just r

canRetry :: MonadIO m => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
  AWS.ServiceError se | se ^. AWS.serviceCode == AWS.ErrorCode "RequestThrottled" -> pure True
  _ -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
