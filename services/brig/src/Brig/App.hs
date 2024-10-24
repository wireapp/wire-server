{-# LANGUAGE DeepSubsumption #-}
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

    -- * App Environment
    Env (..),
    mkIndexEnv,
    newEnv,
    closeEnv,
    providerTemplatesWithLocale,
    teamTemplatesWithLocale,
    teamTemplatesNoLocale,
    cargoholdLens,
    galleyLens,
    galleyEndpointLens,
    gundeckEndpointLens,
    cargoholdEndpointLens,
    federatorLens,
    casClientLens,
    smtpEnvLens,
    emailSenderLens,
    awsEnvLens,
    appLoggerLens,
    internalEventsLens,
    requestIdLens,
    userTemplatesLens,
    providerTemplatesLens,
    teamTemplatesLens,
    templateBrandingLens,
    httpManagerLens,
    http2ManagerLens,
    extGetManagerLens,
    settingsLens,
    fsWatcherLens,
    turnEnvLens,
    sftEnvLens,
    currentTimeLens,
    zauthEnvLens,
    digestSHA256Lens,
    digestMD5Lens,
    indexEnvLens,
    randomPrekeyLocalLockLens,
    keyPackageLocalLockLens,
    rabbitmqChannelLens,
    disabledVersionsLens,
    enableSFTFederationLens,
    readChannel,

    -- * App Monad
    AppT (..),
    viewFederationDomain,
    qualifyLocal,
    qualifyLocal',
    ensureLocal,

    -- * Crutches that should be removed once Brig has been completely transitioned to Polysemy
    wrapClient,
    wrapClientE,
    wrapClientM,
    wrapHttpClient,
    wrapHttpClientE,
    wrapHttp,
    HttpClientIO (..),
    runHttpClientIO,
    liftSem,
    lowerAppT,
    initHttpManagerWithTLSConfig,
    adhocUserKeyStoreInterpreter,
    adhocSessionStoreInterpreter,
  )
where

import Bilge qualified as RPC
import Bilge.IO
import Bilge.RPC (HasRequestId (..))
import Brig.AWS qualified as AWS
import Brig.Calling qualified as Calling
import Brig.DeleteQueue.Interpreter
import Brig.Options (ElasticSearchOpts, Opts, Settings (..))
import Brig.Options qualified as Opt
import Brig.Provider.Template
import Brig.Queue.Stomp qualified as Stomp
import Brig.Queue.Types
import Brig.Schema.Run qualified as Migrations
import Brig.Team.Template
import Brig.Template (Localised, genTemplateBranding)
import Brig.User.Search.Index (IndexEnv (..), MonadIndexIO (..), runIndexIO)
import Brig.User.Template
import Brig.ZAuth (MonadZAuth (..), runZAuth)
import Brig.ZAuth qualified as ZAuth
import Cassandra (runClient)
import Cassandra qualified as Cas
import Cassandra.Util (initCassandraForService)
import Control.AutoUpdate
import Control.Error
import Control.Lens hiding (index, (.=))
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.ByteString.Conversion
import Data.Credentials (Credentials (..))
import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock
import Database.Bloodhound qualified as ES
import HTTP2.Client.Manager (Http2Manager, http2ManagerWithSSLCtx)
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended qualified as Q
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import OpenSSL.EVP.Digest (Digest, getDigestByName)
import OpenSSL.Session (SSLOption (..))
import OpenSSL.Session qualified as SSL
import Polysemy
import Polysemy.Error (throw)
import Polysemy.Error qualified as Polysemy
import Polysemy.Fail
import Polysemy.Final
import Polysemy.Input (Input, input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as P
import Prometheus
import Ssl.Util
import System.FSNotify qualified as FS
import System.Logger.Class hiding (Settings, settings)
import System.Logger.Class qualified as LC
import System.Logger.Extended qualified as Log
import System.Timeout (timeout)
import Util.Options
import Util.SuffixNamer
import Wire.API.Error (errorToWai)
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.Error (federationNotImplemented)
import Wire.API.Locale (Locale)
import Wire.API.Routes.Version
import Wire.API.User.Identity
import Wire.EmailSending.SMTP qualified as SMTP
import Wire.EmailSubsystem.Template (TemplateBranding, forLocale)
import Wire.Error (HttpError (StdError))
import Wire.SessionStore
import Wire.SessionStore.Cassandra
import Wire.UserKeyStore
import Wire.UserKeyStore.Cassandra
import Wire.UserStore
import Wire.UserStore.Cassandra

schemaVersion :: Int32
schemaVersion = Migrations.lastSchemaVersion

-------------------------------------------------------------------------------
-- Environment

data Env = Env
  { cargohold :: RPC.Request,
    galley :: RPC.Request,
    galleyEndpoint :: Endpoint,
    gundeckEndpoint :: Endpoint,
    cargoholdEndpoint :: Endpoint,
    federator :: Maybe Endpoint, -- FUTUREWORK: should we use a better type here? E.g. to avoid fresh connections all the time?
    casClient :: Cas.ClientState,
    smtpEnv :: Maybe SMTP.SMTP,
    emailSender :: EmailAddress,
    awsEnv :: AWS.Env,
    appLogger :: Logger,
    internalEvents :: QueueEnv,
    requestId :: RequestId,
    userTemplates :: Localised UserTemplates,
    providerTemplates :: Localised ProviderTemplates,
    teamTemplates :: Localised TeamTemplates,
    templateBranding :: TemplateBranding,
    httpManager :: Manager,
    http2Manager :: Http2Manager,
    extGetManager :: (Manager, [Fingerprint Rsa] -> SSL.SSL -> IO ()),
    settings :: Settings,
    fsWatcher :: FS.WatchManager,
    turnEnv :: Calling.TurnEnv,
    sftEnv :: Maybe Calling.SFTEnv,
    currentTime :: IO UTCTime,
    zauthEnv :: ZAuth.Env,
    digestSHA256 :: Digest,
    digestMD5 :: Digest,
    indexEnv :: IndexEnv,
    randomPrekeyLocalLock :: Maybe (MVar ()),
    keyPackageLocalLock :: MVar (),
    rabbitmqChannel :: MVar Q.Channel,
    disabledVersions :: Set Version,
    enableSFTFederation :: Maybe Bool
  }

makeLensesWith (lensRules & lensField .~ suffixNamer) ''Env

newEnv :: Opts -> IO Env
newEnv opts = do
  Just md5 <- getDigestByName "MD5"
  Just sha256 <- getDigestByName "SHA256"
  Just sha512 <- getDigestByName "SHA512"
  lgr <- Log.mkLogger (Opt.logLevel opts) (Opt.logNetStrings opts) (Opt.logFormat opts)
  cas <- initCassandra opts lgr
  mgr <- initHttpManager
  h2Mgr <- initHttp2Manager
  ext <- initExtGetManager
  utp <- loadUserTemplates opts
  ptp <- loadProviderTemplates opts
  ttp <- loadTeamTemplates opts
  let branding = genTemplateBranding . Opt.templateBranding . Opt.general . Opt.emailSMS $ opts
  (emailAWSOpts, emailSMTP) <- emailConn lgr $ Opt.email (Opt.emailSMS opts)
  aws <- AWS.mkEnv lgr (Opt.aws opts) emailAWSOpts mgr
  zau <- initZAuth opts
  clock <- mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
  w <-
    FS.startManagerConf $
      FS.defaultConfig {FS.confWatchMode = FS.WatchModeOS}
  let turnOpts = Opt.turn opts
  turnSecret <- Text.encodeUtf8 . Text.strip <$> Text.readFile (Opt.secret turnOpts)
  turn <- Calling.mkTurnEnv (Opt.serversSource turnOpts) (Opt.tokenTTL turnOpts) (Opt.configTTL turnOpts) turnSecret sha512
  eventsQueue :: QueueEnv <- case opts.internalEvents.internalEventsQueue of
    StompQueueOpts q -> do
      stomp :: Stomp.Env <- case (opts.stompOptions, opts.settings.stomp) of
        (Just s, Just c) -> Stomp.mkEnv s <$> initCredentials c
        (Just _, Nothing) -> error "STOMP is configured but 'setStomp' is not set"
        (Nothing, Just _) -> error "'setStomp' is present but STOMP is not configured"
        (Nothing, Nothing) -> error "stomp is selected for internal events, but not configured in 'setStomp', STOMP"
      pure (StompQueueEnv (Stomp.broker stomp) q)
    SqsQueueOpts q -> do
      let throttleMillis = fromMaybe Opt.defSqsThrottleMillis opts.settings.sqsThrottleMillis
      SqsQueueEnv aws throttleMillis <$> AWS.getQueueUrl (aws ^. AWS.amazonkaEnv) q
  mSFTEnv <- mapM (Calling.mkSFTEnv sha512) $ Opt.sft opts
  prekeyLocalLock <- case Opt.randomPrekeys opts of
    Just True -> do
      Log.info lgr $ Log.msg (Log.val "randomPrekeys: active")
      Just <$> newMVar ()
    _ -> do
      Log.info lgr $ Log.msg (Log.val "randomPrekeys: not active; using dynamoDB instead.")
      pure Nothing
  kpLock <- newMVar ()
  rabbitChan <- Q.mkRabbitMqChannelMVar lgr opts.rabbitmq
  let allDisabledVersions = foldMap expandVersionExp opts.settings.disabledAPIVersions
  idxEnv <- mkIndexEnv opts.elasticsearch lgr (Opt.galley opts) mgr
  pure $!
    Env
      { cargohold = mkEndpoint $ opts.cargohold,
        galley = mkEndpoint $ opts.galley,
        galleyEndpoint = opts.galley,
        gundeckEndpoint = opts.gundeck,
        cargoholdEndpoint = opts.cargohold,
        federator = opts.federatorInternal,
        casClient = cas,
        smtpEnv = emailSMTP,
        emailSender = opts.emailSMS.general.emailSender,
        awsEnv = aws, -- used by `journalEvent` directly
        appLogger = lgr,
        internalEvents = (eventsQueue :: QueueEnv),
        requestId = RequestId defRequestId,
        userTemplates = utp,
        providerTemplates = ptp,
        teamTemplates = ttp,
        templateBranding = branding,
        httpManager = mgr,
        http2Manager = h2Mgr,
        extGetManager = ext,
        settings = opts.settings,
        turnEnv = turn,
        sftEnv = mSFTEnv,
        fsWatcher = w,
        currentTime = clock,
        zauthEnv = zau,
        digestMD5 = md5,
        digestSHA256 = sha256,
        indexEnv = idxEnv,
        randomPrekeyLocalLock = prekeyLocalLock,
        keyPackageLocalLock = kpLock,
        rabbitmqChannel = rabbitChan,
        disabledVersions = allDisabledVersions,
        enableSFTFederation = opts.multiSFT
      }
  where
    emailConn _ (Opt.EmailAWS aws) = pure (Just aws, Nothing)
    emailConn lgr (Opt.EmailSMTP s) = do
      let h = s.smtpEndpoint.host
          p = Just . fromInteger . toInteger $ s.smtpEndpoint.port
      smtpCredentials <- case Opt.smtpCredentials s of
        Just (Opt.EmailSMTPCredentials u p') -> do
          Just . (SMTP.Username u,) . SMTP.Password <$> initCredentials p'
        _ -> pure Nothing
      smtp <- SMTP.initSMTP lgr h p smtpCredentials (Opt.smtpConnType s)
      pure (Nothing, Just smtp)
    mkEndpoint service = RPC.host (encodeUtf8 service.host) . RPC.port service.port $ RPC.empty

mkIndexEnv :: ElasticSearchOpts -> Logger -> Endpoint -> Manager -> IO IndexEnv
mkIndexEnv esOpts logger galleyEp rpcHttpManager = do
  mEsCreds :: Maybe Credentials <- for esOpts.credentials initCredentials
  mEsAddCreds :: Maybe Credentials <- for esOpts.additionalCredentials initCredentials

  let mkBhEnv skipVerifyTls mCustomCa mCreds url = do
        mgr <- initHttpManagerWithTLSConfig skipVerifyTls mCustomCa
        let bhe = ES.mkBHEnv url mgr
        pure $ maybe bhe (\creds -> bhe {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername creds.username) (ES.EsPassword creds.password)}) mCreds
      esLogger = Log.clone (Just "index.brig") logger
  bhEnv <- mkBhEnv esOpts.insecureSkipVerifyTls esOpts.caCert mEsCreds esOpts.url
  additionalBhEnv <-
    for esOpts.additionalWriteIndexUrl $
      mkBhEnv esOpts.additionalInsecureSkipVerifyTls esOpts.additionalCaCert mEsAddCreds
  pure $
    IndexEnv
      { idxLogger = esLogger,
        idxElastic = bhEnv,
        idxRequest = Nothing,
        idxName = esOpts.index,
        idxAdditionalName = esOpts.additionalWriteIndex,
        idxAdditionalElastic = additionalBhEnv,
        idxGalley = galleyEp,
        idxRpcHttpManager = rpcHttpManager,
        idxCredentials = mEsCreds
      }

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
  initHttpManagerWithTLSConfig False Nothing

initHttpManagerWithTLSConfig :: Bool -> Maybe FilePath -> IO Manager
initHttpManagerWithTLSConfig skipTlsVerify mCustomCa = do
  -- See Note [SSL context]
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  if skipTlsVerify
    then SSL.contextSetVerificationMode ctx SSL.VerifyNone
    else
      SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
  case mCustomCa of
    Nothing -> SSL.contextSetDefaultVerifyPaths ctx
    Just customCa -> do
      filePath <- canonicalizePath customCa
      SSL.contextSetCAFile ctx filePath
  -- Unfortunately, there are quite some AWS services we talk to
  -- (e.g. SES, Dynamo) that still only support TLSv1.
  -- Ideally: SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  newManager
    (opensslManagerSettings (pure ctx))
      { managerConnCount = 1024,
        managerIdleConnectionCount = 4096,
        managerResponseTimeout = responseTimeoutMicro 10000000
      }

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
  SSL.contextSetDefaultVerifyPaths ctx
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
initCassandra o g =
  initCassandraForService
    (Opt.cassandra o)
    "brig"
    (Opt.discoUrl o)
    (Just schemaVersion)
    g

teamTemplatesWithLocale :: (MonadReader Env m) => Maybe Locale -> m (Locale, TeamTemplates)
teamTemplatesWithLocale l = forLocale l <$> asks (.teamTemplates)

providerTemplatesWithLocale :: (MonadReader Env m) => Maybe Locale -> m (Locale, ProviderTemplates)
providerTemplatesWithLocale l = forLocale l <$> asks (.providerTemplates)

-- this works because team templates is not affected by `forLocale`; it is useful where we
-- use the `TeamTemplates` only for finding invitation url templates (those are not localized).
teamTemplatesNoLocale :: (MonadReader Env m) => m TeamTemplates
teamTemplatesNoLocale = snd <$> teamTemplatesWithLocale Nothing

closeEnv :: Env -> IO ()
closeEnv e = do
  Cas.shutdown $ e.casClient
  FS.stopManager $ e.fsWatcher
  Log.flush $ e.appLogger
  Log.close $ e.appLogger

-------------------------------------------------------------------------------
-- App Monad

newtype AppT r a = AppT
  { unAppT :: (Member (Final IO) r) => ReaderT Env (Sem r) a
  }
  deriving
    ( Semigroup,
      Monoid
    )
    via (Ap (AppT r) a)

lowerAppT :: (Member (Final IO) r) => Env -> AppT r a -> Sem r a
lowerAppT env (AppT r) = runReaderT r env

instance Functor (AppT r) where
  fmap fab (AppT x0) = AppT $ fmap fab x0

instance Applicative (AppT r) where
  pure a = AppT $ pure a
  (AppT x0) <*> (AppT x1) = AppT $ x0 <*> x1

instance Monad (AppT r) where
  (AppT x0) >>= f = AppT $ x0 >>= unAppT . f

instance MonadIO (AppT r) where
  liftIO io = AppT $ lift $ embedFinal io

instance MonadMonitor (AppT r) where
  doIO = liftIO

instance MonadThrow (AppT r) where
  throwM = liftIO . throwM

instance (Member Fail r) => MonadFail (AppT r) where
  fail = AppT . fail

instance (Member (Final IO) r) => MonadThrow (Sem r) where
  throwM = embedFinal . throwM @IO

instance (Member (Final IO) r) => MonadCatch (Sem r) where
  catch m handler = withStrategicToFinal @IO $ do
    m' <- runS m
    st <- getInitialStateS
    handler' <- bindS handler
    pure $ m' `catch` \e -> handler' $ e <$ st

instance MonadCatch (AppT r) where
  catch (AppT m) handler = AppT $
    ReaderT $ \env ->
      catch (runReaderT m env) (\x -> runReaderT (unAppT $ handler x) env)

instance MonadReader Env (AppT r) where
  ask = AppT ask
  local f (AppT m) = AppT $ local f m

liftSem :: Sem r a -> AppT r a
liftSem sem = AppT $ lift sem

instance (MonadIO m) => MonadLogger (ReaderT Env m) where
  log l m = do
    g <- asks (.appLogger)
    r <- asks (.requestId)
    Log.log g l $ field "request" (unRequestId r) ~~ m

instance MonadLogger (AppT r) where
  log l m = do
    g <- asks (.appLogger)
    r <- asks (.requestId)
    AppT $
      lift $
        embedFinal @IO $
          Log.log g l $
            field "request" (unRequestId r) ~~ m

instance MonadLogger (ExceptT err (AppT r)) where
  log l m = lift (LC.log l m)

instance MonadHttp (AppT r) where
  handleRequestWithCont req handler = do
    manager <- asks (.httpManager)
    liftIO $ withResponse req manager handler

instance MonadZAuth (AppT r) where
  liftZAuth za = asks (.zauthEnv) >>= \e -> runZAuth e za

instance MonadZAuth (ExceptT err (AppT r)) where
  liftZAuth za = lift (asks (.zauthEnv)) >>= flip runZAuth za

-- | The function serves as a crutch while Brig is being polysemised. Use it
-- whenever the compiler complains that there is no instance of `MonadClient`
-- for `AppT r`. It can be removed once there is no `AppT` anymore.
wrapClient :: ReaderT Env Cas.Client a -> AppT r a
wrapClient m = do
  env <- ask
  runClient env.casClient $ runReaderT m env

wrapClientE :: ExceptT e (ReaderT Env Cas.Client) a -> ExceptT e (AppT r) a
wrapClientE = mapExceptT wrapClient

wrapClientM :: MaybeT (ReaderT Env Cas.Client) b -> MaybeT (AppT r) b
wrapClientM = mapMaybeT wrapClient

wrapHttp ::
  HttpClientIO a ->
  AppT r a
wrapHttp action = do
  env <- ask
  runHttpClientIO env action

newtype HttpClientIO a = HttpClientIO
  { unHttpClientIO :: ReaderT Env (HttpT Cas.Client) a
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

runHttpClientIO :: (MonadIO m) => Env -> HttpClientIO a -> m a
runHttpClientIO env =
  runClient (env.casClient)
    . runHttpT (env.httpManager)
    . flip runReaderT env
    . unHttpClientIO

instance MonadZAuth HttpClientIO where
  liftZAuth za = asks (.zauthEnv) >>= flip runZAuth za

instance HasRequestId HttpClientIO where
  getRequestId = asks (.requestId)

instance Cas.MonadClient HttpClientIO where
  liftClient cl = do
    env <- ask
    liftIO $ runClient (asks (.casClient) env) cl
  localState f = local (casClientLens %~ f)

instance MonadMonitor HttpClientIO where
  doIO = liftIO

wrapHttpClient ::
  HttpClientIO a ->
  AppT r a
wrapHttpClient = wrapHttp

wrapHttpClientE :: ExceptT e HttpClientIO a -> ExceptT e (AppT r) a
wrapHttpClientE = mapExceptT wrapHttpClient

instance (MonadIO m) => MonadIndexIO (ReaderT Env m) where
  liftIndexIO m = asks (.indexEnv) >>= \e -> runIndexIO e m

instance MonadIndexIO (AppT r) where
  liftIndexIO m = do
    AppT $ mapReaderT (embedToFinal @IO) $ liftIndexIO m

instance (MonadIndexIO (AppT r)) => MonadIndexIO (ExceptT err (AppT r)) where
  liftIndexIO m = asks (.indexEnv) >>= \e -> runIndexIO e m

instance HasRequestId (AppT r) where
  getRequestId = asks (.requestId)

readChannel ::
  ( Member (Embed IO) r,
    Member (Polysemy.Error HttpError) r,
    Member TinyLog r
  ) =>
  MVar Q.Channel ->
  Sem r Q.Channel
readChannel chanMVar = do
  mChan <- liftIO $ timeout 1_000_000 $ readMVar chanMVar
  maybe onNothing pure mChan
  where
    onNothing = do
      P.err $ Log.msg @Text "failed to connect to RabbitMQ"
      throw $ StdError $ errorToWai @'E.NotificationQueueConnectionError

-------------------------------------------------------------------------------
-- Ad hoc interpreters

-- | similarly to `wrapClient`, this function serves as a crutch while Brig is being polysemised.
adhocUserKeyStoreInterpreter :: (MonadIO m, MonadReader Env m) => Sem '[UserKeyStore, UserStore, Embed IO] a -> m a
adhocUserKeyStoreInterpreter action = do
  clientState <- asks (.casClient)
  liftIO $ runM . interpretUserStoreCassandra clientState . interpretUserKeyStoreCassandra clientState $ action

-- | similarly to `wrapClient`, this function serves as a crutch while Brig is being polysemised.
adhocSessionStoreInterpreter :: (MonadIO m, MonadReader Env m) => Sem '[SessionStore, Embed IO] a -> m a
adhocSessionStoreInterpreter action = do
  clientState <- asks (.casClient)
  liftIO $ runM . interpretSessionStoreCassandra clientState $ action

--------------------------------------------------------------------------------
-- Federation

viewFederationDomain :: (MonadReader Env m) => m Domain
viewFederationDomain = asks (.settings.federationDomain)

-- FUTUREWORK: rename to 'qualifyLocalMtl'
qualifyLocal :: (MonadReader Env m) => a -> m (Local a)
qualifyLocal a = toLocalUnsafe <$> viewFederationDomain <*> pure a

-- FUTUREWORK: rename to 'qualifyLocalPoly'
qualifyLocal' :: ((Member (Input (Local ()))) r) => a -> Sem r (Local a)
qualifyLocal' a = flip toLocalUnsafe a . tDomain <$> input

-- | Convert a qualified value into a local one. Throw if the value is not actually local.
ensureLocal :: Qualified a -> AppT r (Local a)
ensureLocal x = do
  loc <- qualifyLocal ()
  foldQualified loc pure (\_ -> throwM federationNotImplemented) x
