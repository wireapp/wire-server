module Testlib.Env where

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as Aeson
import Data.Char
import Data.Functor
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Data.Word
import qualified Data.Yaml as Yaml
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import System.Exit
import System.FilePath
import System.IO
import Testlib.Prekeys

-- | Initialised once per test.
data Env = Env
  { serviceMap :: Map String ServiceMap,
    domain1 :: String,
    domain2 :: String,
    defaultAPIVersion :: Int,
    manager :: HTTP.Manager,
    serviceConfigsDir :: FilePath,
    servicesCwdBase :: Maybe FilePath,
    prekeys :: IORef [(Int, String)],
    lastPrekeys :: IORef [String]
  }

-- | Initialised once per testsuite.
data GlobalEnv = GlobalEnv
  { gServiceMap :: Map String ServiceMap,
    gDomain1 :: String,
    gDomain2 :: String,
    gDefaultAPIVersion :: Int,
    gManager :: HTTP.Manager,
    gServiceConfigsDir :: FilePath,
    gServicesCwdBase :: Maybe FilePath
  }

data IntegrationConfig = IntegrationConfig
  { backendOne :: BackendConfig,
    backendTwo :: BackendConfig
  }

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
    spar :: HostPort
  }
  deriving (Show, Generic)

instance FromJSON ServiceMap

data BackendConfig = BackendConfig
  { beServiceMap :: ServiceMap,
    originDomain :: String
  }
  deriving (Show)

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

data Service = Brig | Galley | Cannon
  deriving (Show, Eq, Ord)

serviceName :: Service -> String
serviceName srv = map toLower (show srv)

serviceHostPort :: ServiceMap -> Service -> HostPort
serviceHostPort m Brig = m.brig
serviceHostPort m Galley = m.galley
serviceHostPort m Cannon = m.cannon

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
        gServicesCwdBase = devEnvProjectRoot <&> (</> "services")
      }

mkEnv :: GlobalEnv -> IO Env
mkEnv ge = do
  pks <- newIORef (zip [1 ..] somePrekeys)
  lpks <- newIORef someLastPrekeys
  pure
    Env
      { serviceMap = gServiceMap ge,
        domain1 = gDomain1 ge,
        domain2 = gDomain2 ge,
        defaultAPIVersion = gDefaultAPIVersion ge,
        manager = gManager ge,
        serviceConfigsDir = gServiceConfigsDir ge,
        servicesCwdBase = gServicesCwdBase ge,
        prekeys = pks,
        lastPrekeys = lpks
      }
