{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module CargoHold.App
    ( -- * Environment
      Env
    , AwsEnv (..)
    , newEnv
    , closeEnv
    , CargoHold.App.aws
    , awsAmazonka
    , metrics
    , appLogger
    , requestId
    , settings

      -- * App Monad
    , AppT
    , App
    , runAppT
    , runAppResourceT

      -- * Handler Monad
    , Handler
    , runHandler
    ) where

import Bilge (MonadHttp, Manager, newManager, RequestId (..))
import Bilge.RPC (HasRequestId (..))
import CargoHold.CloudFront
import CargoHold.Options as Opt
import Control.Applicative
import Control.Error (ExceptT, exceptT)
import Control.Lens (view, makeLenses, set, (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Data.Metrics.Middleware (Metrics)
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Client (ManagerSettings (..), responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities (Error (..), lookupRequestId)
import System.Logger.Class hiding (settings)
import Prelude hiding (log)

import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as Aws
import qualified Bilge
import qualified CargoHold.AWS                as AWS
import qualified Data.Metrics.Middleware      as Metrics
import qualified Network.Wai.Utilities.Server as Server
import qualified Ropes.Aws                    as Aws
import qualified System.Logger                as Log

-------------------------------------------------------------------------------
-- Environment

data Env = Env
    { _aws            :: AwsEnv
    , _awsAmazonka    :: AWS.Env
    , _metrics        :: Metrics
    , _appLogger      :: Logger
    , _httpManager    :: Manager
    , _requestId      :: RequestId
    , _settings       :: Opt.Settings
    }

data AwsEnv = AwsEnv
    { awsEnv     :: Aws.Env
    , s3Config   :: Aws.S3Configuration Aws.NormalQuery
    , s3Bucket   :: Text
    , cloudFront :: CloudFront
    }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
    met  <- Metrics.metrics
    lgr  <- Log.new $ Log.setOutput Log.StdOut
                    . Log.setFormat Nothing
                    $ Log.defSettings
    mgr  <- initHttpManager
    awe  <- initAws o lgr mgr
    ama  <- initAwsAmazonka o lgr mgr
    return $ Env awe ama met lgr mgr mempty (o^.optSettings)

initAwsAmazonka :: Opts -> Logger -> Manager -> IO AWS.Env
initAwsAmazonka o l m = AWS.mkEnv l (o^.optAwsAmazonka) m

initAws :: Opts -> Logger -> Manager -> IO AwsEnv
initAws o l m = do
    let awsOpts = o^.optAws
    amz  <- Aws.newEnv l m $ liftM2 (,) (awsOpts^.awsKeyId) (awsOpts^.awsSecretKey)
    sig  <- initCloudFront (awsOpts^.awsCfPrivateKey) (awsOpts^.awsCfKeyPairId) (awsOpts^.awsCfDomain)
    let s3c  = Aws.s3 Aws.HTTPS Aws.s3EndpointEu False
    return $! AwsEnv amz s3c (awsOpts^.awsS3Bucket) sig

-- TODO: If we want to have more control on the cipher suite, look into
-- https://hackage.haskell.org/package/tls-1.4.0/docs/Network-TLS.html
-- and make use of ClientParams
initHttpManager :: IO Manager
initHttpManager =
    newManager tlsManagerSettings
        { managerConnCount           = 1024
        , managerIdleConnectionCount = 2048
        , managerResponseTimeout     = responseTimeoutMicro 10000000
        }

closeEnv :: Env -> IO ()
closeEnv e = Log.close $ e^.appLogger

-------------------------------------------------------------------------------
-- App Monad

newtype AppT m a = AppT (ReaderT Env m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader Env
             )

type App = AppT IO

instance MonadLogger App where
    log l m = do
        g <- view appLogger
        r <- view requestId
        Log.log g l $ "request" .= unRequestId r ~~ m

instance MonadLogger (ExceptT e App) where
    log l = lift . log l

instance MonadHttp App where
    getManager = view httpManager

instance HasRequestId App where
    getRequestId = view requestId

instance MonadHttp (ExceptT e App) where
    getManager = lift Bilge.getManager

instance HasRequestId (ExceptT e App) where
    getRequestId = lift getRequestId

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT a) = runReaderT a e

runAppResourceT :: MonadIO m => Env -> ResourceT App a -> m a
runAppResourceT e rma = liftIO . runResourceT $ transResourceT (runAppT e) rma

-------------------------------------------------------------------------------
-- Handler Monad

type Handler = ExceptT Error App

runHandler :: Env -> Request -> Handler ResponseReceived -> Continue IO -> IO ResponseReceived
runHandler e r h k =
    let e' = set requestId (maybe mempty RequestId (lookupRequestId r)) e
    in runAppT e' (exceptT (Server.onError (_appLogger e) (_metrics e) r k) return h)

