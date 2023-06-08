module Testlib.Env where

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Pool
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Word
import qualified Data.Yaml as Yaml
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Testlib.Prekeys
import Prelude

-- | Initialised once per test.
data Env = Env
  { serviceMap :: Map String ServiceMap,
    domain1 :: String,
    domain2 :: String,
    defaultAPIVersion :: Int,
    manager :: HTTP.Manager,
    serviceConfigsDir :: FilePath,
    servicesCwdBase :: Maybe FilePath,
    removalKeyPath :: FilePath,
    prekeys :: IORef [(Int, String)],
    lastPrekeys :: IORef [String],
    mls :: IORef MLSState,
    resourcePool :: Pool BackendResource
  }

-- | Initialised once per testsuite.
data GlobalEnv = GlobalEnv
  { gServiceMap :: Map String ServiceMap,
    gDomain1 :: String,
    gDomain2 :: String,
    gDefaultAPIVersion :: Int,
    gManager :: HTTP.Manager,
    gServiceConfigsDir :: FilePath,
    gServicesCwdBase :: Maybe FilePath,
    gRemovalKeyPath :: FilePath
  }

data IntegrationConfig = IntegrationConfig
  { backendOne :: BackendConfig,
    backendTwo :: BackendConfig
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig where
  parseJSON v =
    IntegrationConfig
      <$> parseJSON v
      <*> withObject "ServiceMap at backendTwo" (Aeson..: fromString "backendTwo") v

data ServiceMap = ServiceMap
  { brig :: HostPort,
    cannon :: HostPort,
    cargohold :: HostPort,
    federatorInternal :: HostPort,
    federatorExternal :: HostPort,
    galley :: HostPort,
    gundeck :: HostPort,
    nginz :: HostPort,
    spar :: HostPort,
    proxy :: HostPort
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

data NginzConfig = NginzConfig
  { localPort :: Word16,
    http2Port :: Word16,
    sslPort :: Word16,
    fedPort :: Word16
  }
  deriving (Show, Generic)

data Service = Brig | Galley | Cannon | Gundeck | Cargohold | Nginz | Spar
  deriving
    ( Show,
      Eq,
      Ord,
      Enum,
      Bounded
    )

serviceName :: Service -> String
serviceName srv = map toLower (show srv)

serviceHostPort :: ServiceMap -> Service -> HostPort
serviceHostPort m Brig = m.brig
serviceHostPort m Galley = m.galley
serviceHostPort m Cannon = m.cannon
serviceHostPort m Gundeck = m.gundeck
serviceHostPort m Cargohold = m.cargohold
serviceHostPort m Nginz = m.nginz
serviceHostPort m Spar = m.spar

mkGlobalEnv :: FilePath -> IO GlobalEnv
mkGlobalEnv cfgFile = do
  eith <- Yaml.decodeFileEither cfgFile
  intConfig <- case eith of
    Left err -> do
      hPutStrLn stderr $ "Could not parse " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err
      exitFailure
    Right (intConfig :: IntegrationConfig) -> pure intConfig

  let devEnvProjectRoot = case splitPath (takeDirectory cfgFile) of
        [] -> Nothing
        ps ->
          if last ps == "services"
            then Just (joinPath (init ps))
            else Nothing

  let configsDir =
        case devEnvProjectRoot of
          Just root -> root </> "./services/.integration/A/etc/wire/"
          Nothing -> "/etc/wire"

  manager <- HTTP.newManager HTTP.defaultManagerSettings
  pure
    GlobalEnv
      { gServiceMap =
          Map.fromList
            [ (intConfig.backendOne.originDomain, intConfig.backendOne.beServiceMap),
              (intConfig.backendTwo.originDomain, intConfig.backendTwo.beServiceMap)
            ],
        gDomain1 = intConfig.backendOne.originDomain,
        gDomain2 = intConfig.backendTwo.originDomain,
        gDefaultAPIVersion = 4,
        gManager = manager,
        gServiceConfigsDir = configsDir,
        gServicesCwdBase = devEnvProjectRoot <&> (</> "services"),
        gRemovalKeyPath = error "Uninitialised removal key path"
      }

mkEnv :: GlobalEnv -> Codensity IO Env
mkEnv ge = do
  mls <- liftIO . newIORef =<< mkMLSState
  liftIO $ do
    pks <- newIORef (zip [1 ..] somePrekeys)
    lpks <- newIORef someLastPrekeys
    resources <- newIORef $ backendResources 3
    pool <- createPool (create resources) (destroy resources) 1 120 3
    pure
      Env
        { serviceMap = gServiceMap ge,
          domain1 = gDomain1 ge,
          domain2 = gDomain2 ge,
          defaultAPIVersion = gDefaultAPIVersion ge,
          manager = gManager ge,
          serviceConfigsDir = gServiceConfigsDir ge,
          servicesCwdBase = gServicesCwdBase ge,
          removalKeyPath = gRemovalKeyPath ge,
          prekeys = pks,
          lastPrekeys = lpks,
          mls = mls,
          resourcePool = pool
        }

destroy :: IORef (Set BackendResource) -> BackendResource -> IO ()
destroy ioRef = modifyIORef' ioRef . Set.insert

create :: IORef (Set.Set BackendResource) -> IO BackendResource
create ioRef = do
  resources <- Set.toList <$> readIORef ioRef
  case resources of
    [] -> error "No resources available"
    (r : rs) -> do
      writeIORef ioRef (Set.fromList rs)
      pure r

data MLSState = MLSState
  { baseDir :: FilePath,
    members :: Set ClientIdentity,
    -- | users expected to receive a welcome message after the next commit
    newMembers :: Set ClientIdentity,
    groupId :: Maybe String,
    convId :: Maybe Value,
    clientGroupState :: Map ClientIdentity ByteString,
    epoch :: Word64
  }
  deriving (Show)

mkMLSState :: Codensity IO MLSState
mkMLSState = Codensity $ \k ->
  withSystemTempDirectory "mls" $ \tmp -> do
    k
      MLSState
        { baseDir = tmp,
          members = mempty,
          newMembers = mempty,
          groupId = Nothing,
          convId = Nothing,
          clientGroupState = mempty,
          epoch = 0
        }

data ClientIdentity = ClientIdentity
  { domain :: String,
    user :: String,
    client :: String
  }
  deriving (Show, Eq, Ord)

data BackendResource = BackendResource
  { berBrigKeyspace :: String,
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
    berVHost :: String
  }
  deriving (Show, Eq, Ord)

backendResources :: Word16 -> Set.Set BackendResource
backendResources n =
  [1 .. n]
    <&> ( \i ->
            BackendResource
              { berBrigKeyspace = "brig_test_dyn_" <> show i,
                berGalleyKeyspace = "galley_test_dyn_" <> show i,
                berSparKeyspace = "spar_test_dyn_" <> show i,
                berGundeckKeyspace = "gundeck_test_dyn_" <> show i,
                berElasticsearchIndex = "directory_dyn_" <> show i <> "_test",
                berFederatorInternal = federatorInternalPort i,
                berFederatorExternal = federatorExternalPort i,
                berDomain = domain i,
                berAwsUserJournalQueue = "integration-user-events.fifo" <> suffix i,
                berAwsPrekeyTable = "integration-brig-prekeys" <> suffix i,
                berAwsS3Bucket = "dummy-bucket" <> suffix i,
                berAwsQueueName = "integration-gundeck-events" <> suffix i,
                berBrigInternalEvents = "integration-brig-events-internal" <> suffix i,
                berEmailSMSSesQueue = "integration-brig-events" <> suffix i,
                berEmailSMSEmailSender = "backend-integration" <> suffix i <> "@wire.com",
                berGalleyJournal = "integration-team-events.fifo" <> suffix i,
                berVHost = mkVHost i
              }
        )
    & Set.fromList
  where
    suffix :: Word16 -> String
    suffix i = show $ i + 2

    -- Fixed internal port for federator, e.g. for dynamic backends: 1 -> 10097, 2 -> 11097, etc.
    federatorInternalPort :: Num a => a -> a
    federatorInternalPort i = 8097 + (2 * 1000 * i)

    -- Fixed external port for federator, e.g. for dynamic backends: 1 -> 10098, 2 -> 11098, etc.
    federatorExternalPort :: Num a => a -> a
    federatorExternalPort i = 8098 + (2 * 1000 * i)

    -- Fixed domain for a backend resource, e.g. for dynamic backends: 1 -> "c.example.com", 2 -> "d.example.com", etc.
    domain :: Integral a => a -> String
    domain i = [chr (ord 'c' + fromIntegral i - 1)] <> ".example.com"

    mkVHost :: Integral a => a -> String
    mkVHost = domain
