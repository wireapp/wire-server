{-
NOTE: Don't import any other Testlib modules here. Use this module to break dependency cycles.
-}

module Testlib.Types where

import Control.Concurrent (QSemN)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception as E
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.Random (MonadRandom (..))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.Default
import Data.Functor
import Data.IORef
import Data.List
import Data.Map
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Word
import GHC.Generics (Generic)
import GHC.Records
import GHC.Stack
import qualified Network.AMQP as Q
import Network.AMQP.Extended
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.URI
import UnliftIO (MonadUnliftIO)
import Prelude

data ResourcePool a = ResourcePool
  { sem :: QSemN,
    resources :: IORef (Set.Set a),
    onAcquire :: a -> IO ()
  }

data BackendResource = BackendResource
  { berName :: BackendName,
    berBrigKeyspace :: String,
    berGalleyKeyspace :: String,
    berSparKeyspace :: String,
    berGundeckKeyspace :: String,
    berElasticsearchIndex :: String,
    berPostgresqlDBName :: String,
    berFederatorInternal :: Word16,
    berFederatorExternal :: Word16,
    berDomain :: String,
    berAwsUserJournalQueue :: String,
    berAwsPrekeyTable :: String,
    berAwsS3Bucket :: String,
    berAwsQueueName :: String,
    berBrigInternalEvents :: String,
    berEmailSMSSesQueue :: String,
    berEmailSMSEmailSender :: String,
    berGalleyJournal :: String,
    berVHost :: String,
    berNginzSslPort :: Word16,
    berNginzHttp2Port :: Word16,
    berInternalServicePorts :: forall a. (Num a) => Service -> a,
    -- | A disabled service is started anyway, but not configured in the other services.
    berEnableService :: Service -> Bool,
    berMlsPrivateKeyPaths :: Value
  }

instance Eq BackendResource where
  a == b = a.berName == b.berName

instance Ord BackendResource where
  a `compare` b = a.berName `compare` b.berName

data DynamicBackendConfig = DynamicBackendConfig
  { domain :: String,
    federatorExternalPort :: Word16,
    mlsPrivateKeyPaths :: Value
  }
  deriving (Show, Generic)

instance FromJSON DynamicBackendConfig

data DNSMockServerConfig = DNSMockServerConfig
  { host :: !String,
    apiPort :: !Word16,
    dohPort :: !Word16
  }
  deriving (Show, Generic)

instance FromJSON DNSMockServerConfig

-- | Initialised once per testsuite.
data GlobalEnv = GlobalEnv
  { gServiceMap :: Map String ServiceMap,
    gDomain1 :: String,
    gDomain2 :: String,
    gIntegrationTestHostName :: String,
    gFederationV0Domain :: String,
    gFederationV1Domain :: String,
    gFederationV2Domain :: String,
    gDynamicDomains :: [String],
    gDefaultAPIVersion :: Int,
    gManager :: HTTP.Manager,
    gServicesCwdBase :: Maybe FilePath,
    gBackendResourcePool :: ResourcePool BackendResource,
    gRabbitMQConfig :: RabbitMqAdminOpts,
    gRabbitMQConfigV0 :: RabbitMqAdminOpts,
    gRabbitMQConfigV1 :: RabbitMqAdminOpts,
    gTempDir :: FilePath,
    gTimeOutSeconds :: Int,
    gDNSMockServerConfig :: DNSMockServerConfig,
    gCellsEventQueue :: String,
    gCellsEventWatchersLock :: MVar (),
    gCellsEventWatchers :: IORef (Map String QueueWatcher)
  }

data IntegrationConfig = IntegrationConfig
  { backendOne :: BackendConfig,
    backendTwo :: BackendConfig,
    federationV0 :: BackendConfig,
    federationV1 :: BackendConfig,
    federationV2 :: BackendConfig,
    integrationTestHostName :: String,
    dynamicBackends :: Map String DynamicBackendConfig,
    rabbitmq :: RabbitMqAdminOpts,
    rabbitmqV0 :: RabbitMqAdminOpts,
    rabbitmqV1 :: RabbitMqAdminOpts,
    cassandra :: CassandraConfig,
    dnsMockServer :: DNSMockServerConfig,
    cellsEventQueue :: String
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig where
  parseJSON =
    withObject "IntegrationConfig" $ \o ->
      IntegrationConfig
        <$> parseJSON (Object o)
        <*> o .: fromString "backendTwo"
        <*> o .: fromString "federation-v0"
        <*> o .: fromString "federation-v1"
        <*> o .: fromString "federation-v2"
        <*> o .: fromString "integrationTestHostName"
        <*> o .: fromString "dynamicBackends"
        <*> o .: fromString "rabbitmq"
        <*> o .: fromString "rabbitmq-v0"
        <*> o .: fromString "rabbitmq-v1"
        <*> o .: fromString "cassandra"
        <*> o .: fromString "dnsMockServer"
        <*> o .: fromString "cellsEventQueue"

data ServiceMap = ServiceMap
  { brig :: HostPort,
    backgroundWorker :: HostPort,
    cannon :: HostPort,
    cargohold :: HostPort,
    federatorInternal :: HostPort,
    federatorExternal :: HostPort,
    galley :: HostPort,
    gundeck :: HostPort,
    nginz :: HostPort,
    proxy :: HostPort, -- maps on WireProxy, but we don't want to touch config files.
    spar :: HostPort,
    stern :: HostPort,
    wireServerEnterprise :: HostPort,
    rabbitMqVHost :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON ServiceMap

data BackendConfig = BackendConfig
  { beServiceMap :: ServiceMap,
    originDomain :: String
  }
  deriving (Show, Generic)

instance FromJSON BackendConfig where
  parseJSON v =
    BackendConfig
      <$> parseJSON v
      <*> withObject "BackendConfig" (\ob -> ob .: fromString "originDomain") v

data HostPort = HostPort
  { host :: String,
    port :: Word16
  }
  deriving (Show, Generic)

instance FromJSON HostPort

data CassandraConfig = CassandraConfig
  { cassHost :: String,
    cassPort :: Word16,
    cassTlsCa :: Maybe FilePath
  }
  deriving (Show, Generic)

instance FromJSON CassandraConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = lowerFirst . dropPrefix}
    where
      lowerFirst :: String -> String
      lowerFirst (x : xs) = toLower x : xs
      lowerFirst [] = ""

      dropPrefix :: String -> String
      dropPrefix = Prelude.drop (length "cass")

data QueueWatcher = QueueWatcher
  { doneVar :: MVar (),
    broadcast :: TChan Q.Message
  }

stopQueueWatcher :: QueueWatcher -> IO ()
stopQueueWatcher watcher = void $ tryPutMVar watcher.doneVar ()

-- | Initialised once per test.
data Env = Env
  { serviceMap :: Map String ServiceMap,
    domain1 :: String,
    domain2 :: String,
    integrationTestHostName :: String,
    federationV0Domain :: String,
    federationV1Domain :: String,
    federationV2Domain :: String,
    dynamicDomains :: [String],
    defaultAPIVersion :: Int,
    apiVersionByDomain :: Map String Int,
    manager :: HTTP.Manager,
    servicesCwdBase :: Maybe FilePath,
    prekeys :: IORef [(Int, String)],
    lastPrekeys :: IORef [String],
    mls :: IORef MLSState,
    resourcePool :: ResourcePool BackendResource,
    rabbitMQConfig :: RabbitMqAdminOpts,
    timeOutSeconds :: Int,
    currentTestName :: Maybe String,
    dnsMockServerConfig :: DNSMockServerConfig,
    cellsEventQueue :: String,
    cellsEventWatchersLock :: MVar (),
    cellsEventWatchers :: IORef (Map String QueueWatcher)
  }

data Response = Response
  { jsonBody :: Maybe Aeson.Value,
    body :: ByteString,
    status :: Int,
    headers :: [HTTP.Header],
    request :: HTTP.Request
  }
  deriving stock (Show)

instance HasField "json" Response (App Aeson.Value) where
  getField response = maybe (assertFailure "Response has no json body") pure response.jsonBody

data CredentialType = BasicCredentialType | X509CredentialType
  deriving (Eq, Show)

data ClientIdentity = ClientIdentity
  { domain :: String,
    user :: String,
    client :: String
  }
  deriving stock (Show, Eq, Ord, Generic)

instance HasField "qualifiedUserId" ClientIdentity Aeson.Value where
  getField cid = object [fromString "id" .= cid.user, fromString "domain" .= cid.domain]

newtype Ciphersuite = Ciphersuite {code :: String}
  deriving (Eq, Ord, Show, Generic)

instance Default Ciphersuite where
  def = Ciphersuite "0x0002"

data ClientGroupState = ClientGroupState
  { groups :: Map ConvId ByteString,
    -- | mls-test-cli stores by signature scheme
    keystore :: Map String ByteString,
    credType :: CredentialType
  }
  deriving (Show)

instance Default ClientGroupState where
  def =
    ClientGroupState
      { groups = mempty,
        keystore = mempty,
        credType = BasicCredentialType
      }

csSignatureScheme :: Ciphersuite -> String
csSignatureScheme (Ciphersuite code) = case code of
  "0x0002" -> "ecdsa_secp256r1_sha256"
  "0x0005" -> "ecdsa_secp521r1_sha512"
  "0x0007" -> "ecdsa_secp384r1_sha384"
  _ -> "ed25519"

data MLSProtocol = MLSProtocolMLS | MLSProtocolMixed
  deriving (Eq, Show)

data ConvId = ConvId
  { domain :: String,
    id_ :: String,
    groupId :: Maybe String,
    subconvId :: Maybe String
  }
  deriving (Show, Eq, Ord)

convIdToQidObject :: ConvId -> Value
convIdToQidObject convId = object [fromString "id" .= convId.id_, fromString "domain" .= convId.domain]

instance ToJSON ConvId where
  toJSON = convIdToQidObject

data MLSState = MLSState
  { baseDir :: FilePath,
    convs :: Map ConvId MLSConv,
    clientGroupState :: Map ClientIdentity ClientGroupState
  }
  deriving (Show)

printMLSState :: MLSState -> String
printMLSState MLSState {convs, clientGroupState} =
  "MLSState {"
    <> "convs = "
    <> show convs
    <> ", clientGroupState = "
    <> show (Map.keys clientGroupState)
    <> "}"

data MLSConv = MLSConv
  { members :: Set ClientIdentity,
    -- | users expected to receive a welcome message after the next commit
    newMembers :: Set ClientIdentity,
    memberUsers :: Set Value,
    membersToBeRemoved :: Set ClientIdentity,
    groupId :: String,
    convId :: ConvId,
    epoch :: Word64,
    ciphersuite :: Ciphersuite
  }
  deriving (Show)

showRequest :: HTTP.Request -> String
showRequest r =
  T.unpack (T.decodeUtf8 (HTTP.method r))
    <> " "
    <> uriToString id (HTTP.getUri r) ""

showHeaders :: [HTTP.Header] -> String
showHeaders r =
  intercalate "\n" $
    r <&> \(name, value) ->
      C8.unpack (CI.original name) <> ": " <> C8.unpack value

getRequestBody :: HTTP.Request -> Maybe BS.ByteString
getRequestBody req = case HTTP.requestBody req of
  HTTP.RequestBodyLBS lbs -> pure (L.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> Nothing

data AssertionFailure = AssertionFailure
  { callstack :: CallStack,
    response :: Maybe Response,
    context :: Maybe String,
    msg :: String
  }

instance Show AssertionFailure where
  show (AssertionFailure _ _ _ msg) = "AssertionFailure _ _ _ " <> show msg

instance Exception AssertionFailure where
  displayException (AssertionFailure _ _ _ msg) = msg

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadReader Env,
      MonadBase IO,
      MonadUnliftIO,
      MonadBaseControl IO
    )

instance MonadRandom App where
  getRandomBytes n = liftIO (getRandomBytes n)

runAppWithEnv :: Env -> App a -> IO a
runAppWithEnv e m = runReaderT (unApp m) e

-- | Convert an action in the 'App' monad to an 'IO' action.
appToIO :: App a -> App (IO a)
appToIO action = do
  f <- appToIOKleisli (const action)
  pure $ f ()

appToIOKleisli :: (a -> App b) -> App (a -> IO b)
appToIOKleisli k = do
  env <- ask
  pure $ \a -> runAppWithEnv env (k a)

hoistCodensity :: Codensity IO a -> Codensity App a
hoistCodensity m = Codensity $ \k -> do
  iok <- appToIOKleisli k
  liftIO $ runCodensity m iok

getServiceMap :: (HasCallStack) => String -> App ServiceMap
getServiceMap fedDomain = do
  env <- ask
  assertJust ("Could not find service map for federation domain: " <> fedDomain) (Map.lookup fedDomain env.serviceMap)

getMLSState :: App MLSState
getMLSState = do
  ref <- asks (.mls)
  liftIO $ readIORef ref

modifyMLSState :: (MLSState -> MLSState) -> App ()
modifyMLSState f = do
  ref <- asks (.mls)
  liftIO $ modifyIORef ref f

getBaseDir :: App FilePath
getBaseDir = fmap (.baseDir) getMLSState

data AppFailure = AppFailure String CallStack

instance Show AppFailure where
  show (AppFailure msg _) = msg

instance Exception AppFailure where
  displayException (AppFailure msg _) = msg

instance MonadFail App where
  fail msg = assertFailure ("Pattern matching failure: " <> msg)

assertFailure :: (HasCallStack) => String -> App a
assertFailure msg =
  forceList msg $
    liftIO $
      E.throw (AssertionFailure callStack Nothing Nothing msg)
  where
    forceList [] y = y
    forceList (x : xs) y = seq x (forceList xs y)

assertJust :: (HasCallStack) => String -> Maybe a -> App a
assertJust _ (Just x) = pure x
assertJust msg Nothing = assertFailure msg

assertNothing :: (HasCallStack) => Maybe a -> App ()
assertNothing = maybe (pure ()) $ const $ assertFailure "Maybe value was Just, not Nothing"

addFailureContext :: String -> App a -> App a
addFailureContext ctx = modifyFailureContext (\mCtx0 -> Just $ maybe ctx (\x -> ctx <> "\n" <> x) mCtx0)

modifyFailureContext :: (Maybe String -> Maybe String) -> App a -> App a
modifyFailureContext modContext =
  modifyFailure
    (\e -> e {context = modContext e.context})

modifyFailure :: (AssertionFailure -> AssertionFailure) -> App a -> App a
modifyFailure modifyAssertion action = do
  env <- ask
  liftIO
    ( E.catch
        (runAppWithEnv env action)
        ( \(e :: AssertionFailure) ->
            E.throw (modifyAssertion e)
        )
    )

data ServiceOverrides = ServiceOverrides
  { brigCfg :: Value -> App Value,
    cannonCfg :: Value -> App Value,
    cargoholdCfg :: Value -> App Value,
    galleyCfg :: Value -> App Value,
    gundeckCfg :: Value -> App Value,
    nginzCfg :: Value -> App Value,
    wireProxyCfg :: Value -> App Value,
    sparCfg :: Value -> App Value,
    backgroundWorkerCfg :: Value -> App Value,
    sternCfg :: Value -> App Value,
    federatorInternalCfg :: Value -> App Value,
    wireServerEnterpriseCfg :: Value -> App Value
  }

instance Default ServiceOverrides where
  def = defaultServiceOverrides

instance Semigroup ServiceOverrides where
  a <> b =
    ServiceOverrides
      { brigCfg = brigCfg a >=> brigCfg b,
        cannonCfg = cannonCfg a >=> cannonCfg b,
        cargoholdCfg = cargoholdCfg a >=> cargoholdCfg b,
        galleyCfg = galleyCfg a >=> galleyCfg b,
        gundeckCfg = gundeckCfg a >=> gundeckCfg b,
        nginzCfg = nginzCfg a >=> nginzCfg b,
        wireProxyCfg = wireProxyCfg a >=> wireProxyCfg b,
        sparCfg = sparCfg a >=> sparCfg b,
        backgroundWorkerCfg = backgroundWorkerCfg a >=> backgroundWorkerCfg b,
        sternCfg = sternCfg a >=> sternCfg b,
        federatorInternalCfg = federatorInternalCfg a >=> federatorInternalCfg b,
        wireServerEnterpriseCfg = wireServerEnterpriseCfg a >=> wireServerEnterpriseCfg b
      }

instance Monoid ServiceOverrides where
  mempty = defaultServiceOverrides

defaultServiceOverrides :: ServiceOverrides
defaultServiceOverrides =
  ServiceOverrides
    { brigCfg = pure,
      cannonCfg = pure,
      cargoholdCfg = pure,
      galleyCfg = pure,
      gundeckCfg = pure,
      nginzCfg = pure,
      wireProxyCfg = pure,
      sparCfg = pure,
      backgroundWorkerCfg = pure,
      sternCfg = pure,
      federatorInternalCfg = pure,
      wireServerEnterpriseCfg = pure
    }

lookupConfigOverride :: ServiceOverrides -> Service -> (Value -> App Value)
lookupConfigOverride overrides = \case
  Brig -> overrides.brigCfg
  Cannon -> overrides.cannonCfg
  Cargohold -> overrides.cargoholdCfg
  Galley -> overrides.galleyCfg
  Gundeck -> overrides.gundeckCfg
  Nginz -> overrides.nginzCfg
  WireProxy -> overrides.wireProxyCfg
  Spar -> overrides.sparCfg
  BackgroundWorker -> overrides.backgroundWorkerCfg
  Stern -> overrides.sternCfg
  FederatorInternal -> overrides.federatorInternalCfg
  WireServerEnterprise -> overrides.wireServerEnterpriseCfg

data Service
  = Brig
  | Galley
  | Cannon
  | Gundeck
  | Cargohold
  | Nginz
  | WireProxy -- (`Proxy` is already taken)
  | Spar
  | BackgroundWorker
  | Stern
  | FederatorInternal
  | WireServerEnterprise
  deriving
    ( Show,
      Eq,
      Ord,
      Enum,
      Bounded
    )

serviceName :: Service -> String
serviceName = \case
  Brig -> "brig"
  Galley -> "galley"
  Cannon -> "cannon"
  Gundeck -> "gundeck"
  Cargohold -> "cargohold"
  Nginz -> "nginz"
  WireProxy -> "proxy"
  Spar -> "spar"
  BackgroundWorker -> "backgroundWorker"
  Stern -> "stern"
  FederatorInternal -> "federator"
  WireServerEnterprise -> "wireServerEnterprise"

-- | Converts the service name to kebab-case.
configName :: Service -> String
configName = \case
  Brig -> "brig"
  Galley -> "galley"
  Cannon -> "cannon"
  Gundeck -> "gundeck"
  Cargohold -> "cargohold"
  Nginz -> "nginz"
  WireProxy -> "proxy"
  Spar -> "spar"
  BackgroundWorker -> "background-worker"
  Stern -> "stern"
  FederatorInternal -> "federator"
  WireServerEnterprise -> "wire-server-enterprise"

data BackendName
  = BackendA
  | BackendB
  | -- | The index of dynamic backends begin with 1
    DynamicBackend Int
  deriving (Show, Eq, Ord)

allServices :: [Service]
allServices = [minBound .. maxBound]

newtype TestSuiteReport = TestSuiteReport {cases :: [TestCaseReport]}
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

data TestCaseReport = TestCaseReport
  { name :: String,
    result :: TestResult,
    time :: NominalDiffTime
  }
  deriving (Eq, Show)

data TestResult = TestSuccess | TestFailure String
  deriving (Eq, Show)
