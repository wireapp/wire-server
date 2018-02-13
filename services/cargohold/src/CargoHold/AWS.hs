{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
    , assetBucket

    , Error (..)

      -- * AWS
    , exec
    , execCatch

    ) where

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Concurrent.Lifted (threadDelay)
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Aeson hiding ((.=))
import Data.Foldable (for_)
import Data.Monoid
import Data.Text (Text, isPrefixOf)
import Data.Typeable
import Data.Yaml (FromJSON (..))
import Network.AWS (AWSRequest, Rs)
import Network.AWS.SQS (rmrsMessages)
import Network.AWS.SQS.Types hiding (sqs)
import Network.HTTP.Client (Manager, HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types.Status (status400)
import Network.Mail.Mime
import System.Logger.Class
import Util.Options

import qualified Cargohold.Options       as Opt
import qualified Control.Monad.Trans.AWS as AWST
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Encoding      as Text
import qualified Network.AWS             as AWS
import qualified Network.AWS.Data        as AWS
import qualified Network.AWS.Env         as AWS
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

mkEnv :: Logger -> Opt.AWSAmazonkaOpts -> Manager -> IO Env
mkEnv lgr opts mgr = do
    let g = Logger.clone (Just "aws.cargohold") lgr
    let bucket = Opt.awsAmazonkaS3Bucket opts
    e <- mkAwsEnv g (mkEndpoint S3.s3 (Opt.awsAmazonkaS3Endpoint opts))
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

data Error where
    GeneralError     :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show     Error
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
