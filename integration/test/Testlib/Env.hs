{-# LANGUAGE OverloadedStrings #-}

module Testlib.Env where

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import Network.HTTP.Client qualified as HTTP
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Testlib.Prekeys
import Testlib.ResourcePool
import Testlib.Types
import Prelude

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
  resourcePool <-
    createBackendResourcePool
      intConfig.cassandra.host
      intConfig.cassandra.port
      (Map.elems intConfig.dynamicBackends)
      intConfig.rabbitmq
  pure
    GlobalEnv
      { gServiceMap =
          Map.fromList
            [ (intConfig.backendOne.originDomain, intConfig.backendOne.beServiceMap),
              (intConfig.backendTwo.originDomain, intConfig.backendTwo.beServiceMap)
            ],
        gDomain1 = intConfig.backendOne.originDomain,
        gDomain2 = intConfig.backendTwo.originDomain,
        gDynamicDomains = (.domain) <$> Map.elems intConfig.dynamicBackends,
        gDefaultAPIVersion = 4,
        gManager = manager,
        gServicesCwdBase = devEnvProjectRoot <&> (</> "services"),
        gRemovalKeyPath = error "Uninitialised removal key path",
        gBackendResourcePool = resourcePool,
        gRabbitMQConfig = intConfig.rabbitmq
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
          dynamicDomains = gDynamicDomains ge,
          defaultAPIVersion = gDefaultAPIVersion ge,
          manager = gManager ge,
          servicesCwdBase = gServicesCwdBase ge,
          removalKeyPath = gRemovalKeyPath ge,
          prekeys = pks,
          lastPrekeys = lpks,
          mls = mls,
          resourcePool = ge.gBackendResourcePool,
          rabbitMQConfig = ge.gRabbitMQConfig
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
