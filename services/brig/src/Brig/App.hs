{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Brig.App
    ( schemaVersion

      -- * App Environment
    , Env
    , newEnv
    , closeEnv
    , awsEnv
    , smtpEnv
    , stompEnv
    , cargohold
    , galley
    , gundeck
    , userTemplates
    , providerTemplates
    , teamTemplates
    , requestId
    , httpManager
    , extGetManager
    , nexmoCreds
    , twilioCreds
    , settings
    , currentTime
    , geoDb
    , zauthEnv
    , digestSHA256
    , digestMD5
    , metrics
    , applog
    , turnEnv
    , turnEnvV2
    , internalEvents

      -- * App Monad
    , AppT
    , AppIO
    , runAppT
    , runAppResourceT
    , forkAppIO

    , locationOf
    ) where

import Imports
import Bilge (MonadHttp, Manager, newManager, RequestId (..))
import Bilge.RPC (HasRequestId (..))
import Brig.Options (Opts, Settings)
import Brig.Queue.Types (Queue (..))
import Brig.Template (Localised, forLocale)
import Brig.Provider.Template
import Brig.Team.Template
import Brig.User.Search.Index (runIndexIO, IndexEnv (..), MonadIndexIO (..))
import Brig.User.Template
import Brig.Types (Locale (..), TurnURI)
import Brig.ZAuth (MonadZAuth (..), runZAuth)
import Cassandra (MonadClient (..), Keyspace (..), runClient)
import Cassandra.Schema (versionCheck)
import Control.AutoUpdate
import Control.Error
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((.=), index)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Trans.Resource
import Data.ByteString.Conversion
import Data.Default (def)
import Data.Id (UserId)
import Data.IP
import Data.List1 (list1, List1)
import Data.Metrics (Metrics)
import Data.Misc
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Data.Yaml (FromJSON)
import Network.HTTP.Client (ManagerSettings (..), responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import OpenSSL.EVP.Digest (getDigestByName, Digest)
import OpenSSL.Session (SSLOption (..))
import Ssl.Util
import System.Logger.Class hiding (Settings, settings)
import Util.Options

import qualified Bilge                    as RPC
import qualified Brig.AWS                 as AWS
import qualified Brig.Queue.Stomp         as Stomp
import qualified Brig.Options             as Opt
import qualified Brig.SMTP                as SMTP
import qualified Brig.TURN                as TURN
import qualified Brig.ZAuth               as ZAuth
import qualified Cassandra                as Cas
import qualified Cassandra.Settings       as Cas
import qualified Data.GeoIP2              as GeoIp
import qualified Data.List.NonEmpty       as NE
import qualified Data.Metrics.Middleware  as Metrics
import qualified Database.V5.Bloodhound   as ES
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.IO             as Text
import qualified OpenSSL.Session          as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import qualified Ropes.Nexmo              as Nexmo
import qualified Ropes.Twilio             as Twilio
import qualified System.FilePath          as Path
import qualified System.FSNotify          as FS
import qualified System.Logger            as Log
import qualified System.Logger.Class      as LC

schemaVersion :: Int32
schemaVersion = 56

-------------------------------------------------------------------------------
-- Environment

data Env = Env
    { _cargohold     :: RPC.Request
    , _galley        :: RPC.Request
    , _gundeck       :: RPC.Request
    , _casClient     :: Cas.ClientState
    , _smtpEnv       :: Maybe SMTP.SMTP
    , _awsEnv        :: AWS.Env
    , _stompEnv      :: Maybe Stomp.Env
    , _metrics       :: Metrics
    , _applog        :: Logger
    , _internalEvents :: Queue
    , _requestId     :: RequestId
    , _usrTemplates  :: Localised UserTemplates
    , _provTemplates :: Localised ProviderTemplates
    , _tmTemplates   :: Localised TeamTemplates
    , _httpManager   :: Manager
    , _extGetManager :: (Manager, [Fingerprint Rsa] -> SSL.SSL -> IO ())
    , _settings      :: Settings
    , _nexmoCreds    :: Nexmo.Credentials
    , _twilioCreds   :: Twilio.Credentials
    , _geoDb         :: Maybe (IORef GeoIp.GeoDB)
    , _fsWatcher     :: FS.WatchManager
    , _turnEnv       :: IORef TURN.Env
    , _turnEnvV2     :: IORef TURN.Env
    , _currentTime   :: IO UTCTime
    , _zauthEnv      :: ZAuth.Env
    , _digestSHA256  :: Digest
    , _digestMD5     :: Digest
    , _indexEnv      :: IndexEnv
    }

makeLenses ''Env

mkLogger :: Opts -> IO Logger
mkLogger opts = Log.new $ Log.defSettings
  & Log.setLogLevel (Opt.logLevel opts)
  & Log.setOutput Log.StdOut
  & Log.setFormat Nothing
  & Log.setNetStrings (Opt.logNetStrings opts)

newEnv :: Opts -> IO Env
newEnv o = do
    Just md5 <- getDigestByName "MD5"
    Just sha256 <- getDigestByName "SHA256"
    Just sha512 <- getDigestByName "SHA512"
    mtr <- Metrics.metrics
    lgr <- mkLogger o
    cas <- initCassandra o lgr
    mgr <- initHttpManager
    ext <- initExtGetManager
    utp <- loadUserTemplates o
    ptp <- loadProviderTemplates o
    ttp <- loadTeamTemplates o
    (emailAWSOpts, emailSMTP) <- emailConn lgr $ Opt.email (Opt.emailSMS o)
    aws <- AWS.mkEnv lgr (Opt.aws o) emailAWSOpts mgr
    zau <- initZAuth o
    clock <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    w   <- FS.startManagerConf
         $ FS.defaultConfig { FS.confDebounce = FS.Debounce 0.5, FS.confPollInterval = 10000000 }
    g   <- geoSetup lgr w $ Opt.geoDb o
    (turn, turnV2) <- turnSetup lgr w sha512 (Opt.turn o)
    let sett = Opt.optSettings o
    nxm <- initCredentials (Opt.setNexmo sett)
    twl <- initCredentials (Opt.setTwilio sett)
    stomp <- case (Opt.stomp o, Opt.setStomp sett) of
        (Nothing, Nothing) -> pure Nothing
        (Just s, Just c)   -> Just . Stomp.mkEnv s <$> initCredentials c
        (Just _, Nothing)  -> error "STOMP is configured but 'setStomp' is not set"
        (Nothing, Just _)  -> error "'setStomp' is present but STOMP is not configured"
    -- This is messy. See Note [queue refactoring] to learn how we
    -- eventually plan to solve this mess.
    eventsQueue <- case Opt.internalEventsQueue (Opt.internalEvents o) of
        StompQueue q -> pure (StompQueue q)
        SqsQueue q -> SqsQueue <$> AWS.getQueueUrl (aws ^. AWS.amazonkaEnv) q
    return $! Env
        { _cargohold     = mkEndpoint $ Opt.cargohold o
        , _galley        = mkEndpoint $ Opt.galley o
        , _gundeck       = mkEndpoint $ Opt.gundeck o
        , _casClient     = cas
        , _smtpEnv       = emailSMTP
        , _awsEnv        = aws
        , _stompEnv      = stomp
        , _metrics       = mtr
        , _applog        = lgr
        , _internalEvents = eventsQueue
        , _requestId     = def
        , _usrTemplates  = utp
        , _provTemplates = ptp
        , _tmTemplates   = ttp
        , _httpManager   = mgr
        , _extGetManager = ext
        , _settings      = sett
        , _nexmoCreds    = nxm
        , _twilioCreds   = twl
        , _geoDb         = g
        , _turnEnv       = turn
        , _turnEnvV2     = turnV2
        , _fsWatcher     = w
        , _currentTime   = clock
        , _zauthEnv      = zau
        , _digestMD5     = md5
        , _digestSHA256  = sha256
        , _indexEnv      = mkIndexEnv o lgr mgr mtr
        }
  where
    emailConn _   (Opt.EmailAWS aws) = return (Just aws, Nothing)
    emailConn lgr (Opt.EmailSMTP  s) = do
        let host = (Opt.smtpEndpoint s)^.epHost
            port = Just $ fromInteger $ toInteger $ (Opt.smtpEndpoint s)^.epPort

        smtpCredentials <- case Opt.smtpCredentials s of
            Just (Opt.EmailSMTPCredentials u p) -> do
                pass <- initCredentials p
                return $ Just (SMTP.Username u, SMTP.Password pass)
            _                                   -> return Nothing
        smtp <- SMTP.initSMTP lgr host port smtpCredentials (Opt.smtpConnType s)
        return (Nothing, Just smtp)

    mkEndpoint service = RPC.host (encodeUtf8 (service^.epHost)) . RPC.port (service^.epPort) $ RPC.empty

mkIndexEnv :: Opts -> Logger -> Manager -> Metrics -> IndexEnv
mkIndexEnv o lgr mgr mtr =
    let bhe  = ES.mkBHEnv (ES.Server (Opt.url (Opt.elasticsearch o))) mgr
        lgr' = Log.clone (Just "index.brig") lgr
    in IndexEnv mtr lgr' bhe Nothing (ES.IndexName $ Opt.index (Opt.elasticsearch o))

geoSetup :: Logger -> FS.WatchManager -> Maybe FilePath -> IO (Maybe (IORef GeoIp.GeoDB))
geoSetup _   _ Nothing   = return Nothing
geoSetup lgr w (Just db) = do
    path  <- canonicalizePath db
    geodb <- newIORef =<< GeoIp.openGeoDB path
    startWatching w path (replaceGeoDb lgr geodb)
    return $ Just geodb

turnSetup :: Logger -> FS.WatchManager -> Digest -> Opt.TurnOpts -> IO (IORef TURN.Env, IORef TURN.Env)
turnSetup lgr w dig o = do
    secret <- Text.encodeUtf8 . Text.strip <$> Text.readFile (Opt.secret o)
    cfg    <- setupTurn secret (Opt.servers o)
    cfgV2  <- setupTurn secret (Opt.serversV2 o)
    return (cfg, cfgV2)
  where
    setupTurn secret cfg = do
        path    <- canonicalizePath cfg
        servers <- fromMaybe (error "Empty TURN list, check turn file!") <$> readTurnList path
        te      <- newIORef =<< TURN.newEnv dig servers (Opt.tokenTTL o) (Opt.configTTL o) secret
        startWatching w path (replaceTurnServers lgr te)
        return te

startWatching :: FS.WatchManager -> FilePath -> FS.Action -> IO ()
startWatching w p = void . FS.watchDir w (Path.dropFileName p) predicate
  where
    predicate (FS.Added f _ _)    = Path.equalFilePath f p
    predicate (FS.Modified f _ _) = Path.equalFilePath f p
    predicate (FS.Removed _ _ _)  = False
    predicate (FS.Unknown _ _ _)  = False

replaceGeoDb :: Logger -> IORef GeoIp.GeoDB -> FS.Event -> IO ()
replaceGeoDb g ref e = do
    let logErr x = Log.err g (msg $ val "Error loading GeoIP database: " +++ show x)
    handleAny logErr $ do
        GeoIp.openGeoDB (FS.eventPath e) >>= atomicWriteIORef ref
        Log.info g (msg $ val "New GeoIP database loaded.")

replaceTurnServers :: Logger -> IORef TURN.Env -> FS.Event -> IO ()
replaceTurnServers g ref e = do
    let logErr x = Log.err g (msg $ val "Error loading turn servers: " +++ show x)
    handleAny logErr $ readTurnList (FS.eventPath e) >>= \case
        Just servers -> readIORef ref >>= \old -> do
            atomicWriteIORef ref (old & TURN.turnServers .~ servers)
            Log.info g (msg $ val "New turn servers loaded.")
        Nothing -> Log.warn g (msg $ val "Empty or malformed turn servers list, ignoring!")

initZAuth :: Opts -> IO ZAuth.Env
initZAuth o = do
    let zOpts = Opt.zauth o
        privateKeys = Opt.privateKeys zOpts
        publicKeys = Opt.publicKeys zOpts
    sk <- ZAuth.readKeys privateKeys
    pk <- ZAuth.readKeys publicKeys
    case (sk, pk) of
        (Nothing,     _) -> error ("No private key in: " ++ privateKeys)
        (_,     Nothing) -> error ("No public key in: " ++ publicKeys)
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
    newManager (opensslManagerSettings (pure ctx))
        { managerConnCount           = 1024
        , managerIdleConnectionCount = 4096
        , managerResponseTimeout     = responseTimeoutMicro 10000000
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
    mgr <- newManager (opensslManagerSettings (pure ctx))  -- see Note [SSL context]
        { managerConnCount           = 100
        , managerIdleConnectionCount = 512
        , managerResponseTimeout     = responseTimeoutMicro 10000000
        }
    Just sha <- getDigestByName "SHA256"
    return (mgr, mkVerify sha)
  where
    mkVerify sha fprs =
        let pinset = map toByteString' fprs
        in  verifyRsaFingerprint sha pinset

initCassandra :: Opts -> Logger -> IO Cas.ClientState
initCassandra o g = do
    c <- maybe (Cas.initialContactsPlain ((Opt.cassandra o)^.casEndpoint.epHost))
               (Cas.initialContactsDisco "cassandra_brig")
               (unpack <$> Opt.discoUrl o)
    p <- Cas.init (Log.clone (Just "cassandra.brig") g)
            $ Cas.setContacts (NE.head c) (NE.tail c)
            . Cas.setPortNumber (fromIntegral ((Opt.cassandra o)^.casEndpoint.epPort))
            . Cas.setKeyspace (Keyspace ((Opt.cassandra o)^.casKeyspace))
            . Cas.setMaxConnections 4
            . Cas.setPoolStripes 4
            . Cas.setSendTimeout 3
            . Cas.setResponseTimeout 10
            . Cas.setProtocolVersion Cas.V3
            $ Cas.defSettings
    runClient p $ versionCheck schemaVersion
    return p

initCredentials :: (FromJSON a) => FilePathSecrets -> IO a
initCredentials secretFile = do
    dat <- loadSecret secretFile
    return $ either (\e -> error $ "Could not load secrets from " ++ show secretFile ++ ": " ++ e) id dat

userTemplates :: Monad m => Maybe Locale -> AppT m (Locale, UserTemplates)
userTemplates l = forLocale l <$> view usrTemplates

providerTemplates :: Monad m => Maybe Locale -> AppT m (Locale, ProviderTemplates)
providerTemplates l = forLocale l <$> view provTemplates

teamTemplates :: Monad m => Maybe Locale -> AppT m (Locale, TeamTemplates)
teamTemplates l = forLocale l <$> view tmTemplates

closeEnv :: Env -> IO ()
closeEnv e = do
    Cas.shutdown $ e^.casClient
    FS.stopManager $ e^.fsWatcher
    Log.flush    $ e^.applog
    Log.close    $ e^.applog

-------------------------------------------------------------------------------
-- App Monad
newtype AppT m a = AppT
    { unAppT :: ReaderT Env m a
    } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader Env
             )

type AppIO = AppT IO

instance MonadIO m => MonadLogger (AppT m) where
    log l m = do
        g <- view applog
        r <- view requestId
        Log.log g l $ field "request" (unRequestId r) ~~ m

instance MonadIO m => MonadLogger (ExceptT err (AppT m)) where
    log l m = lift (LC.log l m)

instance Monad m => MonadHttp (AppT m) where
    getManager = view httpManager

instance MonadIO m => MonadZAuth (AppT m) where
    liftZAuth za = view zauthEnv >>= \e -> runZAuth e za

instance MonadIO m => MonadZAuth (ExceptT err (AppT m)) where
    liftZAuth = lift . liftZAuth

instance (MonadThrow m, MonadCatch m, MonadIO m) => MonadClient (AppT m) where
   liftClient m = view casClient >>= \c -> runClient c m
   localState f = local (over casClient f)

instance MonadIndexIO AppIO where
    liftIndexIO m = view indexEnv >>= \e -> runIndexIO e m

instance Monad m => HasRequestId (AppT m) where
    getRequestId = view requestId

instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
    withRunInIO inner =
      AppT $ ReaderT $ \r ->
      withRunInIO $ \run ->
      inner (run . flip runReaderT r . unAppT)

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

runAppResourceT :: ResourceT AppIO a -> AppIO a
runAppResourceT ma = do
    e <- ask
    liftIO . runResourceT $ transResourceT (runAppT e) ma

forkAppIO :: Maybe UserId -> AppIO a -> AppIO ()
forkAppIO u ma = do
    a <- ask
    g <- view applog
    r <- view requestId
    let logErr e = Log.err g $ request r ~~ user u ~~ msg (show e)
    void . liftIO . forkIO $
        either logErr (const $ return ()) =<<
            runExceptT (syncIO $ runAppT a ma)
  where
    request = field "request" . unRequestId
    user    = maybe id (field "user" . toByteString)

locationOf :: (MonadIO m, MonadReader Env m) => IP -> m (Maybe Location)
locationOf ip = view geoDb >>= \case
    Just g -> do
        database <- liftIO $ readIORef g
        return $! do
            loc <- GeoIp.geoLocation =<< hush (GeoIp.findGeoData database "en" ip)
            return $ location (Latitude $ GeoIp.locationLatitude loc) (Longitude $ GeoIp.locationLongitude loc)
    Nothing -> return Nothing

readTurnList :: FilePath -> IO (Maybe (List1 TurnURI))
readTurnList = Text.readFile >=> return . fn . mapMaybe fromByteString . fmap Text.encodeUtf8 . Text.lines
  where
    fn []     = Nothing
    fn (x:xs) = Just (list1 x xs)
