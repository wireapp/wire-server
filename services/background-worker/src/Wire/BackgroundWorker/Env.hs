{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Cassandra (ClientState)
import qualified Cassandra as Cass
import qualified Cassandra.Settings as Cass
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Domain (Domain)
import HTTP2.Client.Manager
import Imports
import Network.HTTP.Client
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified System.Logger as Log
import System.Logger.Class
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.BackgroundWorker.Options
import Galley.Cassandra.Code
import Galley.Cassandra.Team
import Data.Misc
import Galley.Types.Teams
import qualified Galley.Aws as Aws
import Data.Range
import Wire.API.Team.Member
import Control.Lens.Combinators (lens)
import Galley.Intra.Util
import Data.Id
import Galley.Monad
import Galley.Env (initExtEnv, ExtEnv (..))

data Env = Env
  { manager :: Manager,
    http2Manager :: Http2Manager,
    logger :: Logger,
    federatorInternal :: Endpoint,
    localDomain :: Domain,
    cassandra' :: ClientState,
    galleyConversationCodeUri :: HttpsUrl,
    legalHoldFlag :: FeatureLegalHold,
    awsEnv :: Maybe Aws.Env,
    currentFanoutLimit :: Range 1 HardTruncationLimit Int32,
    brig' :: Endpoint,
    spar' :: Endpoint,
    gundeck' :: Endpoint,
    requestId' :: RequestId,
    deleteConvThrottle :: Maybe Int,
    extGetManager' :: (Manager, [Fingerprint Rsa] -> SSL.SSL -> IO ())
  }

instance HasCodeStoreEnv Env where
  conversationCodeUri = Wire.BackgroundWorker.Env.galleyConversationCodeUri

instance HasFeatureFlagLegalHold Env where
  getFlagLegalHold = Wire.BackgroundWorker.Env.legalHoldFlag

instance HasAwsEnv Env where
  getAwsEnv = awsEnv

instance HasCurrentFanoutLimit Env where
  getCurrentFanoutLimit = Wire.BackgroundWorker.Env.currentFanoutLimit

instance HasManager Env where
  manager' = lens manager (\e m -> e { manager = m } )

instance HasIntraComponentEndpoints Env where
  brig = lens brig' (\s a -> s { brig' = a } )
  spar = lens spar' (\s a -> s { spar' = a } )
  gundeck = lens gundeck' (\s a -> s { gundeck' = a } )

instance HasLogger Env where
  logger' = lens logger (\s a -> s { logger = a })

instance HasRequestId' Env where
  requestId = lens requestId' (\s a -> s { requestId' = a })

instance DeleteConvThrottle Env where
  deleteConvThrottleMillis = deleteConvThrottle

instance HasCassandra Env where
  cassandra = lens cassandra' (\s a -> s { cassandra' = a })

instance HasExtGetManager Env where
  getExtGetManager = extGetManager'

mkEnv :: Opts -> IO Env
mkEnv opts = do
  manager <- newManager defaultManagerSettings
  http2Manager <- initHttp2Manager
  logger <- Log.mkLogger opts.logLevel Nothing opts.logFormat
  let federatorInternal = opts.federatorInternal
      localDomain = opts.localDomain
      galleyConversationCodeUri = opts.galleyConversationCodeUri
      awsEnv = Nothing
      legalHoldFlag = opts.legalHoldFlag
      currentFanoutLimit = opts.currentFanoutLimit
      brig' = opts.brig
      spar' = opts.spar
      gundeck' = opts.gundeck
      requestId' = opts.requestId
      deleteConvThrottle = Nothing
  cassandra' <- Cass.init $ Cass.defSettings -- TODO: Update these settings
  extGetManager' <- _extGetManager <$> initExtEnv
  pure Env {..}

initHttp2Manager :: IO Http2Manager
initHttp2Manager = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextSetDefaultVerifyPaths ctx
  http2ManagerWithSSLCtx ctx

newtype AppT m a where
  AppT :: {unAppT :: ReaderT Env m a} -> AppT m a
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadReader Env,
      MonadTrans
    )

deriving newtype instance MonadBase b m => MonadBase b (AppT m)

deriving newtype instance MonadBaseControl b m => MonadBaseControl b (AppT m)

instance MonadIO m => MonadLogger (AppT m) where
  log lvl m = do
    l <- asks logger
    Log.log l lvl m

runAppT :: Env -> AppT m a -> m a
runAppT env app = runReaderT (unAppT app) env
