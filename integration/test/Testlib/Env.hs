{-# LANGUAGE OverloadedStrings #-}

module Testlib.Env where

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Aeson hiding ((.=))
import Data.ByteString (ByteString)
import Data.Functor
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Word
import Data.Yaml qualified as Yaml
import GHC.Generics
import Network.HTTP.Client qualified as HTTP
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Testlib.Prekeys
import Testlib.ResourcePool
import Prelude

-- | Initialised once per test.
data Env = Env
  { serviceMap :: Map String ServiceMap,
    domain1 :: String,
    domain2 :: String,
    defaultAPIVersion :: Int,
    manager :: HTTP.Manager,
    servicesCwdBase :: Maybe FilePath,
    removalKeyPath :: FilePath,
    prekeys :: IORef [(Int, String)],
    lastPrekeys :: IORef [String],
    mls :: IORef MLSState,
    resourcePool :: ResourcePool BackendResource
  }

-- | Initialised once per testsuite.
data GlobalEnv = GlobalEnv
  { gServiceMap :: Map String ServiceMap,
    gDomain1 :: String,
    gDomain2 :: String,
    gDefaultAPIVersion :: Int,
    gManager :: HTTP.Manager,
    gServicesCwdBase :: Maybe FilePath,
    gRemovalKeyPath :: FilePath,
    gBackendResourcePool :: ResourcePool BackendResource
  }

data IntegrationConfig = IntegrationConfig
  { backendOne :: BackendConfig,
    backendTwo :: BackendConfig,
    dynamicBackends :: Map String DynamicBackendConfig
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig where
  parseJSON =
    withObject "IntegrationConfig" $ \o ->
      IntegrationConfig
        <$> parseJSON (Object o)
        <*> o .: "backendTwo"
        <*> o .: "dynamicBackends"

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

serviceHostPort :: ServiceMap -> Service -> HostPort
serviceHostPort m Brig = m.brig
serviceHostPort m Galley = m.galley
serviceHostPort m Cannon = m.cannon
serviceHostPort m Gundeck = m.gundeck
serviceHostPort m Cargohold = m.cargohold
serviceHostPort m Nginz = m.nginz
serviceHostPort m Spar = m.spar
serviceHostPort m BackgroundWorker = m.backgroundWorker
serviceHostPort m Stern = m.stern
serviceHostPort m FederatorInternal = m.federatorInternal

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

  manager <- HTTP.newManager HTTP.defaultManagerSettings
  resourcePool <- createBackendResourcePool (Map.elems intConfig.dynamicBackends)
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
        gServicesCwdBase = devEnvProjectRoot <&> (</> "services"),
        gRemovalKeyPath = error "Uninitialised removal key path",
        gBackendResourcePool = resourcePool
      }

mkEnv :: GlobalEnv -> Codensity IO Env
mkEnv ge = do
  mls <- liftIO . newIORef =<< mkMLSState
  liftIO $ do
    pks <- newIORef (zip [1 ..] somePrekeys)
    lpks <- newIORef someLastPrekeys
    pure
      Env
        { serviceMap = gServiceMap ge,
          domain1 = gDomain1 ge,
          domain2 = gDomain2 ge,
          defaultAPIVersion = gDefaultAPIVersion ge,
          manager = gManager ge,
          servicesCwdBase = gServicesCwdBase ge,
          removalKeyPath = gRemovalKeyPath ge,
          prekeys = pks,
          lastPrekeys = lpks,
          mls = mls,
          resourcePool = ge.gBackendResourcePool
        }

destroy :: IORef (Set BackendResource) -> BackendResource -> IO ()
destroy ioRef = modifyIORef' ioRef . Set.insert

create :: IORef (Set.Set BackendResource) -> IO BackendResource
create ioRef =
  atomicModifyIORef
    ioRef
    $ \s ->
      case Set.minView s of
        Nothing -> error "No resources available"
        Just (r, s') -> (s', r)

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
