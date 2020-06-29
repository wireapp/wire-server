{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    Amazon,
    amazonkaEnv,
    execute,
    s3Bucket,
    cloudFront,
    Error (..),

    -- * AWS
    exec,
    execCatch,
    throwA,
    sendCatch,
    canRetry,
    retry5x,
    send,
  )
where

import Blaze.ByteString.Builder (toLazyByteString)
import CargoHold.CloudFront
import CargoHold.Error
import CargoHold.Options
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import qualified Control.Monad.Trans.AWS as AWST
import Control.Monad.Trans.Resource
import Control.Retry
import Imports
import Network.AWS (AWSRequest, Rs)
import qualified Network.AWS as AWS
import qualified Network.AWS.Env as AWS
import qualified Network.AWS.S3 as S3
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import qualified System.Logger as Logger
import System.Logger.Class
import Util.Options

data Env = Env
  { _logger :: !Logger,
    _s3Bucket :: !Text,
    _amazonkaEnv :: !AWS.Env,
    _cloudFront :: !(Maybe CloudFront)
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

instance MonadLogger Amazon where
  log l m = view logger >>= \g -> Logger.log g l m

instance MonadUnliftIO Amazon where
  askUnliftIO = Amazon $ ReaderT $ \r ->
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
mkEnv lgr s3End _s3Download bucket cfOpts mgr = do
  -- TODO: we need to respect _s3Download!
  let g = Logger.clone (Just "aws.cargohold") lgr
  e <- mkAwsEnv g (mkEndpoint S3.s3 s3End)
  cf <- mkCfEnv cfOpts
  return (Env g bucket e cf)
  where
    mkCfEnv (Just o) = Just <$> initCloudFront (o ^. cfPrivateKey) (o ^. cfKeyPairId) 300 (o ^. cfDomain)
    mkCfEnv Nothing = return Nothing
    mkEndpoint svc e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) svc
    mkAwsEnv g s3 =
      set AWS.envLogger (awsLogger g)
        <$> AWS.newEnvWith AWS.Discover Nothing mgr
        <&> AWS.configure s3
    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString
    mapLevel AWS.Info = Logger.Info
    mapLevel AWS.Debug = Logger.Trace
    mapLevel AWS.Trace = Logger.Trace
    mapLevel AWS.Error = Logger.Debug

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = throwA =<< sendCatch r

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) return

execCatch ::
  (AWSRequest a, AWS.HasEnv r, MonadUnliftIO m, MonadCatch m, MonadThrow m) =>
  r ->
  a ->
  m (Either AWS.Error (Rs a))
execCatch e cmd =
  runResourceT . AWST.runAWST e
    $ AWST.trying AWS._Error
    $ AWST.send cmd

exec ::
  (AWSRequest a, AWS.HasEnv r, MonadUnliftIO m, MonadCatch m, MonadThrow m) =>
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
