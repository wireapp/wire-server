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
    , metrics
    , appLogger
    , requestId
    , maxTotalUpload

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
import CargoHold.Options as O
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
import Network.HTTP.Client.OpenSSL
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities (Error (..), lookupRequestId)
import OpenSSL.Session (SSLContext, SSLOption (..))
import System.Logger.Class hiding (settings)
import Prelude hiding (log)

import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as Aws
import qualified Bilge
import qualified Data.Metrics.Middleware      as Metrics
import qualified Network.Wai.Utilities.Server as Server
import qualified OpenSSL.Session              as SSL
import qualified OpenSSL.X509.SystemStore     as SSL
import qualified Ropes.Aws                    as Aws
import qualified System.Logger                as Log

-------------------------------------------------------------------------------
-- Environment

data Env = Env
    { _aws            :: AwsEnv
    , _metrics        :: Metrics
    , _appLogger      :: Logger
    , _httpManager    :: Manager
    , _requestId      :: RequestId
    , _maxTotalUpload :: Int
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
    return $ Env awe met lgr mgr mempty (o^.settings.setMaxTotalBytes)

initAws :: Opts -> Logger -> Manager -> IO AwsEnv
initAws o l m = do
    -- TODO: The AWS package can also load them from the env, check the latest API
    -- https://hackage.haskell.org/package/aws-0.17.1/docs/src/Aws-Core.html#loadCredentialsFromFile
    -- which would avoid the need to specify them in a config file when running tests
    let awsOpts = o^.(O.aws)
    amz  <- Aws.newEnv l m $ liftM2 (,) (awsOpts^.awsKeyId) (awsOpts^.awsSecretKey)
    sig  <- initCloudFront (awsOpts^.awsCfPrivateKey) (awsOpts^.awsCfKeyPairId) (awsOpts^.awsCfDomain)
    let s3c  = Aws.s3 Aws.HTTPS Aws.s3EndpointEu False
    return $! AwsEnv amz s3c (awsOpts^.awsS3Bucket) sig

initHttpManager :: IO Manager
initHttpManager =
    newManager (opensslManagerSettings initSSLContext)
        { managerConnCount           = 1024
        , managerIdleConnectionCount = 2048
        , managerResponseTimeout     = responseTimeoutMicro 10000000
        }

initSSLContext :: IO SSLContext
initSSLContext = do
    ctx <- SSL.context
    SSL.contextAddOption ctx SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL_OP_NO_SSLv3
    SSL.contextSetCiphers ctx "HIGH"
    SSL.contextLoadSystemCerts ctx
    SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
    return ctx

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

