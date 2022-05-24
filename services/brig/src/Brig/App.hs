{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
-- FUTUREWORK: Get rid of this option once Polysemy is fully introduced to Brig
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Brig.App
  ( schemaVersion,
    BrigCanonicalEffects,

    -- * App Environment
    Env,
    newEnv,
    closeEnv,
    awsEnv,
    smtpEnv,
    stompEnv,
    cargohold,
    galley,
    gundeck,
    federator,
    userTemplates,
    providerTemplates,
    teamTemplates,
    templateBranding,
    requestId,
    httpManager,
    extGetManager,
    nexmoCreds,
    twilioCreds,
    settings,
    currentTime,
    geoDb,
    zauthEnv,
    digestSHA256,
    digestMD5,
    metrics,
    applog,
    turnEnv,
    sftEnv,
    internalEvents,
    emailSender,
    randomPrekeyLocalLock,
    keyPackageLocalLock,
    fsWatcher,

    -- * App Monad
    AppT,
    runAppT,
    locationOf,
    viewFederationDomain,
    qualifyLocal,

    -- * Crutches that should be removed once Brig has been completely

    -- * transitioned to Polysemy
    wrapClient,
    wrapClientE,
    wrapClientM,
    wrapHttpClient,
    wrapHttpClientE,
    wrapHttp,
    HttpClientIO (..),
    liftSem,
  )
where

import Bilge (RequestId (..))
import qualified Bilge as RPC
import Bilge.IO
import Bilge.RPC (HasRequestId (..))
import qualified Brig.AWS as AWS
import qualified Brig.Calling as Calling
import Brig.Options (Opts, Settings)
import qualified Brig.Options as Opt
import Brig.Provider.Template
import qualified Brig.Queue.Stomp as Stomp
import Brig.Queue.Types (Queue (..))
import qualified Brig.SMTP as SMTP
import Brig.Sem.CodeStore (CodeStore)
import Brig.Sem.CodeStore.Cassandra
import Brig.Sem.PasswordResetStore (PasswordResetStore)
import Brig.Sem.PasswordResetStore.CodeStore
import Brig.Sem.PasswordResetSupply (PasswordResetSupply)
import Brig.Sem.PasswordResetSupply.IO
import Brig.Sem.UserQuery (UserQuery)
import Brig.Sem.UserQuery.Cassandra
import Brig.Team.Template
import Brig.Template (Localised, TemplateBranding, forLocale, genTemplateBranding)
import Brig.Types (Locale (..))
import Brig.User.Search.Index (IndexEnv (..), MonadIndexIO (..), runIndexIO)
import Brig.User.Template
import Brig.ZAuth (MonadZAuth (..), runZAuth)
import qualified Brig.ZAuth as ZAuth
import Cassandra (Keyspace (Keyspace), runClient)
import qualified Cassandra as Cas
import Cassandra.Schema (versionCheck)
import qualified Cassandra.Settings as Cas
import Control.AutoUpdate
import Control.Error
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding (index, (.=))
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.ByteString.Conversion
import Data.Default (def)
import Data.Domain
import qualified Data.GeoIP2 as GeoIp
import Data.IP
import qualified Data.List.NonEmpty as NE
import Data.Metrics (Metrics)
import qualified Data.Metrics.Middleware as Metrics
import Data.Misc
import Data.Qualified
import Data.Text (unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock
import Data.Yaml (FromJSON)
import qualified Database.Bloodhound as ES
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import OpenSSL.EVP.Digest (Digest, getDigestByName)
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import Polysemy
import Polysemy.Final
import qualified Polysemy.TinyLog as P
import qualified Ropes.Nexmo as Nexmo
import qualified Ropes.Twilio as Twilio
import Ssl.Util
import qualified System.FSNotify as FS
import qualified System.FilePath as Path
import System.Logger.Class hiding (Settings, settings)
import qualified System.Logger.Class as LC
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.API.User.Identity (Email)
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO

schemaVersion :: Int32
schemaVersion = 70

-------------------------------------------------------------------------------
-- Environment

data Env = Env
  { _cargohold :: RPC.Request,
    _galley :: RPC.Request,
    _gundeck :: RPC.Request,
    _federator :: Maybe Endpoint, -- FUTUREWORK: should we use a better type here? E.g. to avoid fresh connections all the time?
    _casClient :: Cas.ClientState,
    _smtpEnv :: Maybe SMTP.SMTP,
    _emailSender :: Email,
    _awsEnv :: AWS.Env,
    _stompEnv :: Maybe Stomp.Env,
    _metrics :: Metrics,
    _applog :: Logger,
    _internalEvents :: Queue,
    _requestId :: RequestId,
    _usrTemplates :: Localised UserTemplates,
    _provTemplates :: Localised ProviderTemplates,
    _tmTemplates :: Localised TeamTemplates,
    _templateBranding :: TemplateBranding,
    _httpManager :: Manager,
    _extGetManager :: (Manager, [Fingerprint Rsa] -> SSL.SSL -> IO ()),
    _settings :: Settings,
    _nexmoCreds :: Nexmo.Credentials,
    _twilioCreds :: Twilio.Credentials,
    _geoDb :: Maybe (IORef GeoIp.GeoDB),
    _fsWatcher :: FS.WatchManager,
    _turnEnv :: Calling.TurnEnv,
    _sftEnv :: Maybe Calling.SFTEnv,
    _currentTime :: IO UTCTime,
    _zauthEnv :: ZAuth.Env,
    _digestSHA256 :: Digest,
    _digestMD5 :: Digest,
    _indexEnv :: IndexEnv,
    _randomPrekeyLocalLock :: Maybe (MVar ()),
    _keyPackageLocalLock :: MVar ()
  }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
  Just md5 <- getDigestByName "MD5"
  Just sha256 <- getDigestByName "SHA256"
  Just sha512 <- getDigestByName "SHA512"
  mtr <- Metrics.metrics
  lgr <- Log.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  cas <- initCassandra o lgr
  mgr <- initHttpManager
  ext <- initExtGetManager
  utp <- loadUserTemplates o
  ptp <- loadProviderTemplates o
  ttp <- loadTeamTemplates o
  let branding = genTemplateBranding . Opt.templateBranding . Opt.general . Opt.emailSMS $ o
  (emailAWSOpts, emailSMTP) <- emailConn lgr $ Opt.email (Opt.emailSMS o)
  aws <- AWS.mkEnv lgr (Opt.aws o) emailAWSOpts mgr
  zau <- initZAuth o
  clock <- mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
  w <-
    FS.startManagerConf $
      FS.defaultConfig {FS.confDebounce = FS.Debounce 0.5, FS.confPollInterval = 10000000}
  g <- geoSetup lgr w $ Opt.geoDb o
  let turnOpts = Opt.turn o
  turnSecret <- Text.encodeUtf8 . Text.strip <$> Text.readFile (Opt.secret turnOpts)
  turn <- Calling.mkTurnEnv (Opt.serversSource turnOpts) (Opt.tokenTTL turnOpts) (Opt.configTTL turnOpts) turnSecret sha512
  let sett = Opt.optSettings o
  nxm <- initCredentials (Opt.setNexmo sett)
  twl <- initCredentials (Opt.setTwilio sett)
  stomp <- case (Opt.stomp o, Opt.setStomp sett) of
    (Nothing, Nothing) -> pure Nothing
    (Just s, Just c) -> Just . Stomp.mkEnv s <$> initCredentials c
    (Just _, Nothing) -> error "STOMP is configured but 'setStomp' is not set"
    (Nothing, Just _) -> error "'setStomp' is present but STOMP is not configured"
  -- This is messy. See Note [queue refactoring] to learn how we
  -- eventually plan to solve this mess.
  eventsQueue <- case Opt.internalEventsQueue (Opt.internalEvents o) of
    StompQueue q -> pure (StompQueue q)
    SqsQueue q -> SqsQueue <$> AWS.getQueueUrl (aws ^. AWS.amazonkaEnv) q
  mSFTEnv <- mapM Calling.mkSFTEnv $ Opt.sft o
  prekeyLocalLock <- case Opt.randomPrekeys o of
    Just True -> Just <$> newMVar ()
    _ -> pure Nothing
  kpLock <- newMVar ()
  pure
    $! Env
      { _cargohold = mkEndpoint $ Opt.cargohold o,
        _galley = mkEndpoint $ Opt.galley o,
        _gundeck = mkEndpoint $ Opt.gundeck o,
        _federator = Opt.federatorInternal o,
        _casClient = cas,
        _smtpEnv = emailSMTP,
        _emailSender = Opt.emailSender . Opt.general . Opt.emailSMS $ o,
        _awsEnv = aws,
        _stompEnv = stomp,
        _metrics = mtr,
        _applog = lgr,
        _internalEvents = eventsQueue,
        _requestId = def,
        _usrTemplates = utp,
        _provTemplates = ptp,
        _tmTemplates = ttp,
        _templateBranding = branding,
        _httpManager = mgr,
        _extGetManager = ext,
        _settings = sett,
        _nexmoCreds = nxm,
        _twilioCreds = twl,
        _geoDb = g,
        _turnEnv = turn,
        _sftEnv = mSFTEnv,
        _fsWatcher = w,
        _currentTime = clock,
        _zauthEnv = zau,
        _digestMD5 = md5,
        _digestSHA256 = sha256,
        _indexEnv = mkIndexEnv o lgr mgr mtr (Opt.galley o),
        _randomPrekeyLocalLock = prekeyLocalLock,
        _keyPackageLocalLock = kpLock
      }
  where
    emailConn _ (Opt.EmailAWS aws) = pure (Just aws, Nothing)
    emailConn lgr (Opt.EmailSMTP s) = do
      let host = Opt.smtpEndpoint s ^. epHost
          port = Just $ fromInteger $ toInteger $ Opt.smtpEndpoint s ^. epPort
      smtpCredentials <- case Opt.smtpCredentials s of
        Just (Opt.EmailSMTPCredentials u p) -> do
          pass <- initCredentials p
          pure $ Just (SMTP.Username u, SMTP.Password pass)
        _ -> pure Nothing
      smtp <- SMTP.initSMTP lgr host port smtpCredentials (Opt.smtpConnType s)
      pure (Nothing, Just smtp)
    mkEndpoint service = RPC.host (encodeUtf8 (service ^. epHost)) . RPC.port (service ^. epPort) $ RPC.empty

mkIndexEnv :: Opts -> Logger -> Manager -> Metrics -> Endpoint -> IndexEnv
mkIndexEnv o lgr mgr mtr galleyEndpoint =
  let bhe = ES.mkBHEnv (ES.Server (Opt.url (Opt.elasticsearch o))) mgr
      lgr' = Log.clone (Just "index.brig") lgr
      mainIndex = ES.IndexName $ Opt.index (Opt.elasticsearch o)
      additionalIndex = ES.IndexName <$> Opt.additionalWriteIndex (Opt.elasticsearch o)
      additionalBhe = flip ES.mkBHEnv mgr . ES.Server <$> Opt.additionalWriteIndexUrl (Opt.elasticsearch o)
   in IndexEnv mtr lgr' bhe Nothing mainIndex additionalIndex additionalBhe galleyEndpoint mgr

geoSetup :: Logger -> FS.WatchManager -> Maybe FilePath -> IO (Maybe (IORef GeoIp.GeoDB))
geoSetup _ _ Nothing = pure Nothing
geoSetup lgr w (Just db) = do
  path <- canonicalizePath db
  geodb <- newIORef =<< GeoIp.openGeoDB path
  startWatching w path (replaceGeoDb lgr geodb)
  pure $ Just geodb

startWatching :: FS.WatchManager -> FilePath -> FS.Action -> IO ()
startWatching w p = void . FS.watchDir w (Path.dropFileName p) predicate
  where
    predicate (FS.Added f _ _) = Path.equalFilePath f p
    predicate (FS.Modified f _ _) = Path.equalFilePath f p
    predicate FS.Removed {} = False
    predicate FS.Unknown {} = False

replaceGeoDb :: Logger -> IORef GeoIp.GeoDB -> FS.Event -> IO ()
replaceGeoDb g ref e = do
  let logErr x = Log.err g (msg $ val "Error loading GeoIP database: " +++ show x)
  handleAny logErr $ do
    GeoIp.openGeoDB (FS.eventPath e) >>= atomicWriteIORef ref
    Log.info g (msg $ val "New GeoIP database loaded.")

initZAuth :: Opts -> IO ZAuth.Env
initZAuth o = do
  let zOpts = Opt.zauth o
      privateKeys = Opt.privateKeys zOpts
      publicKeys = Opt.publicKeys zOpts
  sk <- ZAuth.readKeys privateKeys
  pk <- ZAuth.readKeys publicKeys
  case (sk, pk) of
    (Nothing, _) -> error ("No private key in: " ++ privateKeys)
    (_, Nothing) -> error ("No public key in: " ++ publicKeys)
    (Just s, Just p) -> ZAuth.mkEnv s p $ Opt.authSettings zOpts

initHttpManager :: IO Manager
initHttpManager = do
  -- See Note [SSL context]
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  -- Unfortunately, there are quite some AWS services we talk to
  -- (e.g. SES, Dynamo) that still only support TLSv1.
  -- Ideally: SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  newManager
    (opensslManagerSettings (pure ctx))
      { managerConnCount = 1024,
        managerIdleConnectionCount = 4096,
        managerResponseTimeout = responseTimeoutMicro 10000000
      }

-- Note [SSL context]
-- ~~~~~~~~~~~~
--
-- 'openSslManagerSettings' takes an IO action for creating the context;
-- presumably a new context is created for each connection, then. However,
-- judging by comments at https://github.com/snoyberg/http-client/pull/227,
-- it should be fine to reuse the context, and reusing it is probably
-- faster. So, we reuse the context.

-- TODO: somewhat duplicates Galley.App.initExtEnv
initExtGetManager :: IO (Manager, [Fingerprint Rsa] -> SSL.SSL -> IO ())
initExtGetManager = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx rsaCiphers
  -- We use public key pinning with service providers and want to
  -- support self-signed certificates as well, hence 'VerifyNone'.
  SSL.contextSetVerificationMode ctx SSL.VerifyNone
  SSL.contextLoadSystemCerts ctx
  mgr <-
    newManager
      (opensslManagerSettings (pure ctx)) -- see Note [SSL context]
        { managerConnCount = 100,
          managerIdleConnectionCount = 512,
          managerResponseTimeout = responseTimeoutMicro 10000000
        }
  Just sha <- getDigestByName "SHA256"
  pure (mgr, mkVerify sha)
  where
    mkVerify sha fprs =
      let pinset = map toByteString' fprs
       in verifyRsaFingerprint sha pinset

initCassandra :: Opts -> Logger -> IO Cas.ClientState
initCassandra o g = do
  c <-
    maybe
      (Cas.initialContactsPlain (Opt.cassandra o ^. casEndpoint . epHost))
      (Cas.initialContactsDisco "cassandra_brig")
      (unpack <$> Opt.discoUrl o)
  p <-
    Cas.init $
      Cas.setLogger (Cas.mkLogger (Log.clone (Just "cassandra.brig") g))
        . Cas.setContacts (NE.head c) (NE.tail c)
        . Cas.setPortNumber (fromIntegral (Opt.cassandra o ^. casEndpoint . epPort))
        . Cas.setKeyspace (Keyspace (Opt.cassandra o ^. casKeyspace))
        . Cas.setMaxConnections 4
        . Cas.setPoolStripes 4
        . Cas.setSendTimeout 3
        . Cas.setResponseTimeout 10
        . Cas.setProtocolVersion Cas.V4
        . Cas.setPolicy (Cas.dcFilterPolicyIfConfigured g (Opt.cassandra o ^. casFilterNodesByDatacentre))
        $ Cas.defSettings
  runClient p $ versionCheck schemaVersion
  pure p

initCredentials :: (FromJSON a) => FilePathSecrets -> IO a
initCredentials secretFile = do
  dat <- loadSecret secretFile
  pure $ either (\e -> error $ "Could not load secrets from " ++ show secretFile ++ ": " ++ e) id dat

userTemplates :: MonadReader Env m => Maybe Locale -> m (Locale, UserTemplates)
userTemplates l = forLocale l <$> view usrTemplates

providerTemplates :: MonadReader Env m => Maybe Locale -> m (Locale, ProviderTemplates)
providerTemplates l = forLocale l <$> view provTemplates

teamTemplates :: MonadReader Env m => Maybe Locale -> m (Locale, TeamTemplates)
teamTemplates l = forLocale l <$> view tmTemplates

closeEnv :: Env -> IO ()
closeEnv e = do
  Cas.shutdown $ e ^. casClient
  FS.stopManager $ e ^. fsWatcher
  Log.flush $ e ^. applog
  Log.close $ e ^. applog

-------------------------------------------------------------------------------
-- App Monad

type BrigCanonicalEffects =
  '[ UserQuery,
     PasswordResetStore,
     Now,
     PasswordResetSupply,
     CodeStore,
     P.TinyLog,
     Embed Cas.Client,
     Embed IO,
     Final IO
   ]

newtype AppT r a = AppT
  { unAppT :: Member (Final IO) r => ReaderT Env (Sem r) a
  }
  deriving
    ( Semigroup,
      Monoid
    )
    via (Ap (AppT r) a)

instance Functor (AppT r) where
  fmap fab (AppT x0) = AppT $ fmap fab x0

instance Applicative (AppT r) where
  pure a = AppT $ pure a
  (AppT x0) <*> (AppT x1) = AppT $ x0 <*> x1

instance Monad (AppT r) where
  (AppT x0) >>= f = AppT $ x0 >>= unAppT . f

instance MonadIO (AppT r) where
  liftIO io = AppT $ lift $ embedFinal io

instance MonadThrow (AppT r) where
  throwM = liftIO . throwM

instance Member (Final IO) r => MonadThrow (Sem r) where
  throwM = embedFinal . throwM @IO

instance Member (Final IO) r => MonadCatch (Sem r) where
  catch m handler = withStrategicToFinal @IO $ do
    m' <- runS m
    st <- getInitialStateS
    handler' <- bindS handler
    pure $ m' `catch` \e -> handler' $ e <$ st

instance MonadCatch (AppT r) where
  catch (AppT m) handler = AppT $
    ReaderT $ \env ->
      catch (runReaderT m env) (flip runReaderT env . unAppT . handler)

instance MonadReader Env (AppT r) where
  ask = AppT ask
  local f (AppT m) = AppT $ local f m

liftSem :: Sem r a -> AppT r a
liftSem sem = AppT $ lift sem

instance MonadIO m => MonadLogger (ReaderT Env m) where
  log l m = do
    g <- view applog
    r <- view requestId
    Log.log g l $ field "request" (unRequestId r) ~~ m

instance MonadLogger (AppT r) where
  log l m = do
    g <- view applog
    r <- view requestId
    AppT $
      lift $
        embedFinal @IO $
          Log.log g l $ field "request" (unRequestId r) ~~ m

instance MonadLogger (ExceptT err (AppT r)) where
  log l m = lift (LC.log l m)

instance MonadIO m => MonadHttp (AppT r) where
  handleRequestWithCont req handler = do
    manager <- view httpManager
    liftIO $ withResponse req manager handler

instance MonadZAuth (AppT r) where
  liftZAuth za = view zauthEnv >>= \e -> runZAuth e za

instance MonadZAuth (ExceptT err (AppT r)) where
  liftZAuth za = lift (view zauthEnv) >>= flip runZAuth za

-- | The function serves as a crutch while Brig is being polysemised. Use it
-- whenever the compiler complains that there is no instance of `MonadClient`
-- for `AppT r`. It can be removed once there is no `AppT` anymore.
wrapClient :: ReaderT Env Cas.Client a -> AppT r a
wrapClient m = do
  c <- view casClient
  env <- ask
  runClient c $ runReaderT m env

wrapClientE :: ExceptT e (ReaderT Env Cas.Client) a -> ExceptT e (AppT r) a
wrapClientE = mapExceptT wrapClient

wrapClientM :: MaybeT (ReaderT Env Cas.Client) b -> MaybeT (AppT r) b
wrapClientM = mapMaybeT wrapClient

wrapHttp ::
  HttpClientIO a ->
  AppT r a
wrapHttp (HttpClientIO m) = do
  c <- view casClient
  env <- ask
  manager <- view httpManager
  liftIO . runClient c . runHttpT manager $ runReaderT m env

newtype HttpClientIO a = HttpClientIO
  { runHttpClientIO :: ReaderT Env (HttpT Cas.Client) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadLogger,
      MonadHttp,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadUnliftIO,
      MonadIndexIO
    )

instance MonadZAuth HttpClientIO where
  liftZAuth za = view zauthEnv >>= flip runZAuth za

instance HasRequestId HttpClientIO where
  getRequestId = view requestId

instance Cas.MonadClient HttpClientIO where
  liftClient cl = do
    env <- ask
    liftIO $ runClient (view casClient env) cl
  localState f = local (casClient %~ f)

wrapHttpClient ::
  HttpClientIO a ->
  AppT r a
wrapHttpClient = wrapHttp

wrapHttpClientE :: ExceptT e HttpClientIO a -> ExceptT e (AppT r) a
wrapHttpClientE = mapExceptT wrapHttpClient

instance MonadIO m => MonadIndexIO (ReaderT Env m) where
  liftIndexIO m = view indexEnv >>= \e -> runIndexIO e m

instance MonadIndexIO (AppT r) where
  liftIndexIO m = do
    AppT $ mapReaderT (embedToFinal @IO) $ liftIndexIO m

instance MonadIndexIO (AppT r) => MonadIndexIO (ExceptT err (AppT r)) where
  liftIndexIO m = view indexEnv >>= \e -> runIndexIO e m

instance Monad m => HasRequestId (AppT r) where
  getRequestId = view requestId

runAppT :: Env -> AppT BrigCanonicalEffects a -> IO a
runAppT e (AppT ma) =
  runFinal
    . embedToFinal
    . interpretClientToIO (_casClient e)
    . loggerToTinyLogReqId (view requestId e) (view applog e)
    . codeStoreToCassandra @Cas.Client
    . passwordResetSupplyToIO
    . nowToIOAction (_currentTime e)
    . passwordResetStoreToCodeStore
    . userQueryToCassandra @Cas.Client
    $ runReaderT ma e

locationOf :: (MonadIO m, MonadReader Env m) => IP -> m (Maybe Location)
locationOf ip =
  view geoDb >>= \case
    Just g -> do
      database <- liftIO $ readIORef g
      pure $! do
        loc <- GeoIp.geoLocation =<< hush (GeoIp.findGeoData database "en" ip)
        pure $ location (Latitude $ GeoIp.locationLatitude loc) (Longitude $ GeoIp.locationLongitude loc)
    Nothing -> pure Nothing

--------------------------------------------------------------------------------
-- Federation

viewFederationDomain :: MonadReader Env m => m Domain
viewFederationDomain = view (settings . Opt.federationDomain)

qualifyLocal :: MonadReader Env m => a -> m (Local a)
qualifyLocal a = toLocalUnsafe <$> viewFederationDomain <*> pure a
