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
    , newEnv
    , closeEnv
    , cloudFront
    , aws
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

import Bilge (MonadHttp, Manager, RequestId (..))
import Bilge.RPC (HasRequestId (..))
import CargoHold.CloudFront
import CargoHold.Options as Opt
import Control.Applicative
import Control.Error (ExceptT, exceptT)
import Control.Lens (view, makeLenses, set, (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Data.Default
import Data.Metrics.Middleware (Metrics)
import Data.Monoid
import Network.HTTP.Client (ManagerSettings (..), responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Connection as NC
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities (Error (..), lookupRequestId)
import System.X509 (getSystemCertificateStore)
import System.Logger.Class hiding (settings)
import Prelude hiding (log)

import qualified Bilge
import qualified CargoHold.AWS                as AWS
import qualified Data.Metrics.Middleware      as Metrics
import qualified Network.TLS                  as TLS
import qualified Network.TLS.Extra            as TLS
import qualified Network.Wai.Utilities.Server as Server
import qualified System.Logger                as Log

-------------------------------------------------------------------------------
-- Environment

data Env = Env
    { _aws         :: AWS.Env
    , _cloudFront  :: CloudFront
    , _metrics     :: Metrics
    , _appLogger   :: Logger
    , _httpManager :: Manager
    , _requestId   :: RequestId
    , _settings    :: Opt.Settings
    }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
    met  <- Metrics.metrics
    lgr  <- Log.new $ Log.setOutput Log.StdOut
                    . Log.setFormat Nothing
                    $ Log.defSettings
    mgr  <- initHttpManager
    let awsOpts = o^.optAws
    sig  <- initCloudFront (awsOpts^.awsCfPrivateKey) (awsOpts^.awsCfKeyPairId) (awsOpts^.awsCfDomain)
    ama  <- initAws o lgr mgr
    return $ Env ama sig met lgr mgr mempty (o^.optSettings)

initAws :: Opts -> Logger -> Manager -> IO AWS.Env
initAws o l m = AWS.mkEnv l (o^.optAws.awsS3Endpoint) (o^.optAws.awsS3Bucket) m

-- TODO: If we want to have more control on the cipher suite, look into
-- https://hackage.haskell.org/package/tls-1.4.0/docs/Network-TLS.html
-- and make use of ClientParams
initHttpManager :: IO Manager
initHttpManager = do
    cs <- getSystemCertificateStore
    let tlsClientParams = (TLS.defaultParamsClient "" mempty)
                         { TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
                         , TLS.clientShared    = def
                             { TLS.sharedCAStore = cs
                             , TLS.sharedValidationCache = def
                             }
                         }
    let manSettings = mkManagerSettings (NC.TLSSettings tlsClientParams) Nothing
    mgr <- newTlsManagerWith manSettings
        { managerConnCount           = 1024
        , managerIdleConnectionCount = 2048
        , managerResponseTimeout     = responseTimeoutMicro 10000000
        }
    return mgr

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

