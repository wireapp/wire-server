{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module CargoHold.AWS
    ( -- * Monad
      Env
    , mkEnv
    , Amazon
    , amazonkaEnv
    , execute
    , s3Bucket

    , Error (..)

      -- * AWS
    , exec
    , execCatch
    , throwA
    , sendCatch
    , canRetry
    , retry5x
    , send

    ) where

import Blaze.ByteString.Builder (toLazyByteString)
import CargoHold.Error
import Control.Lens hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Monoid
import Data.Text (Text)
import Network.AWS (AWSRequest, Rs)
import Network.HTTP.Client (Manager, HttpException (..), HttpExceptionContent (..))
import System.Logger.Class
import Util.Options

import qualified Control.Monad.Trans.AWS as AWST
import qualified Network.AWS             as AWS
import qualified Network.AWS.Env         as AWS
import qualified Network.AWS.S3          as S3
import qualified System.Logger           as Logger

data Env = Env
    { _logger         :: !Logger
    , _s3Bucket       :: !Text
    , _amazonkaEnv    :: !AWS.Env
    }

makeLenses ''Env

newtype Amazon a = Amazon
    { unAmazon :: ReaderT Env (ResourceT IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadBase IO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader Env
               , MonadResource
               )

instance MonadLogger Amazon where
    log l m = view logger >>= \g -> Logger.log g l m

instance MonadBaseControl IO Amazon where
    type StM Amazon a = StM (ReaderT Env (ResourceT IO)) a
    liftBaseWith    f = Amazon $ liftBaseWith $ \run -> f (run . unAmazon)
    restoreM          = Amazon . restoreM

instance AWS.MonadAWS Amazon where
    liftAWS a = view amazonkaEnv >>= flip AWS.runAWS a

mkEnv :: Logger -> AWSEndpoint -> Text -> Manager -> IO Env
mkEnv lgr s3End bucket mgr = do
    let g = Logger.clone (Just "aws.cargohold") lgr
    e <- mkAwsEnv g (mkEndpoint S3.s3 s3End)
    return (Env g bucket e)
  where
    mkEndpoint svc e = AWS.setEndpoint (e^.awsSecure) (e^.awsHost) (e^.awsPort) svc

    mkAwsEnv g s3 =  set AWS.envLogger (awsLogger g)
                 <$> AWS.newEnvWith AWS.Discover Nothing mgr
                 <&> AWS.configure s3

    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString

    mapLevel AWS.Info  = Logger.Info
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

execCatch :: (AWSRequest a, AWS.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m)
          => r -> a -> m (Either AWS.Error (Rs a))
execCatch e cmd = runResourceT . AWST.runAWST e
                $ AWST.trying AWS._Error $ AWST.send cmd

exec :: (AWSRequest a, AWS.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m)
     => r -> a -> m (Rs a)
exec e cmd = execCatch e cmd >>= either (throwM . GeneralError) return

canRetry :: MonadIO m => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left  e) = case e of
    AWS.TransportError (HttpExceptionRequest _ ResponseTimeout)                   -> pure True
    AWS.ServiceError se | se^.AWS.serviceCode == AWS.ErrorCode "RequestThrottled" -> pure True
    _                                                                             -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
