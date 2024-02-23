{-
NOTE: Don't import any other Testlib modules here. Use this module to break dependency cycles.
-}

module Testlib.Types where

import Control.Concurrent (QSemN)
import Control.Exception as E
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
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
    berInternalServicePorts :: forall a. Num a => Service -> a
  }

instance Eq BackendResource where
  a == b = a.berName == b.berName

instance Ord BackendResource where
  a `compare` b = a.berName `compare` b.berName

data DynamicBackendConfig = DynamicBackendConfig
  { domain :: String,
    federatorExternalPort :: Word16
  }
  deriving (Show, Generic)

instance FromJSON DynamicBackendConfig

data RabbitMQConfig = RabbitMQConfig
  { host :: String,
    adminPort :: Word16
  }
  deriving (Show)

instance FromJSON RabbitMQConfig where
  parseJSON =
    withObject "RabbitMQConfig" $ \ob ->
      RabbitMQConfig
        <$> ob .: fromString "host"
        <*> ob .: fromString "adminPort"

-- | Initialised once per testsuite.
data GlobalEnv = GlobalEnv
  { gServiceMap :: Map String ServiceMap,
    gDomain1 :: String,
    gDomain2 :: String,
    gFederationV0Domain :: String,
    gDynamicDomains :: [String],
    gDefaultAPIVersion :: Int,
    gManager :: HTTP.Manager,
    gServicesCwdBase :: Maybe FilePath,
    gRemovalKeyPath :: FilePath,
    gBackendResourcePool :: ResourcePool BackendResource,
    gRabbitMQConfig :: RabbitMQConfig,
    gTempDir :: FilePath,
    gTimeOutSeconds :: Int
  }

data IntegrationConfig = IntegrationConfig
  { backendOne :: BackendConfig,
    backendTwo :: BackendConfig,
    federationV0 :: BackendConfig,
    dynamicBackends :: Map String DynamicBackendConfig,
    rabbitmq :: RabbitMQConfig,
    cassandra :: CassandraConfig
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig where
  parseJSON =
    withObject "IntegrationConfig" $ \o ->
      IntegrationConfig
        <$> parseJSON (Object o)
        <*> o .: fromString "backendTwo"
        <*> o .: fromString "federation-v0"
        <*> o .: fromString "dynamicBackends"
        <*> o .: fromString "rabbitmq"
        <*> o .: fromString "cassandra"

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
    spar :: HostPort,
    proxy :: HostPort,
    stern :: HostPort
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

-- | Initialised once per test.
data Env = Env
  { serviceMap :: Map String ServiceMap,
    domain1 :: String,
    domain2 :: String,
    federationV0Domain :: String,
    dynamicDomains :: [String],
    defaultAPIVersion :: Int,
    manager :: HTTP.Manager,
    servicesCwdBase :: Maybe FilePath,
    removalKeyPath :: FilePath,
    prekeys :: IORef [(Int, String)],
    lastPrekeys :: IORef [String],
    mls :: IORef MLSState,
    resourcePool :: ResourcePool BackendResource,
    rabbitMQConfig :: RabbitMQConfig,
    timeOutSeconds :: Int
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

data ClientIdentity = ClientIdentity
  { domain :: String,
    user :: String,
    client :: String
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype Ciphersuite = Ciphersuite {code :: String}
  deriving (Eq, Ord, Show, Generic)

instance Default Ciphersuite where
  def = Ciphersuite "0x0001"

data ClientGroupState = ClientGroupState
  { group :: Maybe ByteString,
    keystore :: Maybe ByteString
  }
  deriving (Show)

data MLSProtocol = MLSProtocolMLS | MLSProtocolMixed
  deriving (Eq, Show)

data MLSState = MLSState
  { baseDir :: FilePath,
    members :: Set ClientIdentity,
    -- | users expected to receive a welcome message after the next commit
    newMembers :: Set ClientIdentity,
    groupId :: Maybe String,
    convId :: Maybe Value,
    clientGroupState :: Map ClientIdentity ClientGroupState,
    epoch :: Word64,
    ciphersuite :: Ciphersuite,
    protocol :: MLSProtocol
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
    msg :: String
  }

instance Show AssertionFailure where
  show (AssertionFailure _ _ msg) = "AssertionFailure _ _ " <> show msg

instance Exception AssertionFailure where
  displayException (AssertionFailure _ _ msg) = msg

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

getServiceMap :: HasCallStack => String -> App ServiceMap
getServiceMap fedDomain = do
  env <- ask
  assertJust ("Could not find service map for federation domain: " <> fedDomain) (Map.lookup fedDomain env.serviceMap)

getMLSState :: App MLSState
getMLSState = do
  ref <- asks (.mls)
  liftIO $ readIORef ref

setMLSState :: MLSState -> App ()
setMLSState s = do
  ref <- asks (.mls)
  liftIO $ writeIORef ref s

modifyMLSState :: (MLSState -> MLSState) -> App ()
modifyMLSState f = do
  ref <- asks (.mls)
  liftIO $ modifyIORef ref f

getBaseDir :: App FilePath
getBaseDir = fmap (.baseDir) getMLSState

data AppFailure = AppFailure String

instance Show AppFailure where
  show (AppFailure msg) = msg

instance Exception AppFailure where
  displayException (AppFailure msg) = msg

instance MonadFail App where
  fail msg = assertFailure ("Pattern matching failure: " <> msg)

assertFailure :: HasCallStack => String -> App a
assertFailure msg =
  forceList msg $
    liftIO $
      E.throw (AssertionFailure callStack Nothing msg)
  where
    forceList [] y = y
    forceList (x : xs) y = seq x (forceList xs y)

assertJust :: HasCallStack => String -> Maybe a -> App a
assertJust _ (Just x) = pure x
assertJust msg Nothing = assertFailure msg

assertNothing :: (HasCallStack) => Maybe a -> App ()
assertNothing = maybe (pure ()) $ const $ assertFailure "Maybe value was Just, not Nothing"

addFailureContext :: String -> App a -> App a
addFailureContext msg = modifyFailureMsg (\m -> m <> "\nThis failure happened in this context:\n" <> msg)

modifyFailureMsg :: (String -> String) -> App a -> App a
modifyFailureMsg modMessage = modifyFailure (\e -> e {msg = modMessage e.msg})

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
    sparCfg :: Value -> App Value,
    backgroundWorkerCfg :: Value -> App Value,
    sternCfg :: Value -> App Value,
    federatorInternalCfg :: Value -> App Value
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
        sparCfg = sparCfg a >=> sparCfg b,
        backgroundWorkerCfg = backgroundWorkerCfg a >=> backgroundWorkerCfg b,
        sternCfg = sternCfg a >=> sternCfg b,
        federatorInternalCfg = federatorInternalCfg a >=> federatorInternalCfg b
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
      sparCfg = pure,
      backgroundWorkerCfg = pure,
      sternCfg = pure,
      federatorInternalCfg = pure
    }

lookupConfigOverride :: ServiceOverrides -> Service -> (Value -> App Value)
lookupConfigOverride overrides = \case
  Brig -> overrides.brigCfg
  Cannon -> overrides.cannonCfg
  Cargohold -> overrides.cargoholdCfg
  Galley -> overrides.galleyCfg
  Gundeck -> overrides.gundeckCfg
  Nginz -> overrides.nginzCfg
  Spar -> overrides.sparCfg
  BackgroundWorker -> overrides.backgroundWorkerCfg
  Stern -> overrides.sternCfg
  FederatorInternal -> overrides.federatorInternalCfg

data Service = Brig | Galley | Cannon | Gundeck | Cargohold | Nginz | Spar | BackgroundWorker | Stern | FederatorInternal
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
  Spar -> "spar"
  BackgroundWorker -> "backgroundWorker"
  Stern -> "stern"
  FederatorInternal -> "federator"

-- | Converts the service name to kebab-case.
configName :: Service -> String
configName = \case
  Brig -> "brig"
  Galley -> "galley"
  Cannon -> "cannon"
  Gundeck -> "gundeck"
  Cargohold -> "cargohold"
  Nginz -> "nginz"
  Spar -> "spar"
  BackgroundWorker -> "background-worker"
  Stern -> "stern"
  FederatorInternal -> "federator"

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
