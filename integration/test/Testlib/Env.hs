{-# LANGUAGE OverloadedStrings #-}

module Testlib.Env where

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Default
import Data.Function ((&))
import Data.Functor
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import qualified Data.Yaml as Yaml
import qualified Database.CQL.IO as Cassandra
import qualified Network.HTTP.Client as HTTP
import qualified OpenSSL.Session as OpenSSL
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Testlib.Prekeys
import Testlib.ResourcePool
import Testlib.Types
import Text.Read (readMaybe)
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

mkGlobalEnv :: FilePath -> Codensity IO GlobalEnv
mkGlobalEnv cfgFile = do
  eith <- liftIO $ Yaml.decodeFileEither cfgFile
  intConfig <- liftIO $ case eith of
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
      getCassCertFilePath :: IO (Maybe FilePath) =
        maybe
          (pure Nothing)
          ( \certFilePath ->
              if isAbsolute certFilePath
                then pure $ Just certFilePath
                else for devEnvProjectRoot $ \projectRoot -> makeAbsolute $ combine projectRoot certFilePath
          )
          intConfig.cassandra.cassTlsCa

  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings

  mbCassCertFilePath <- liftIO $ getCassCertFilePath
  mbSSLContext <- liftIO $ createSSLContext mbCassCertFilePath
  let basicCassSettings =
        Cassandra.defSettings
          & Cassandra.setContacts intConfig.cassandra.cassHost []
          & Cassandra.setPortNumber (fromIntegral intConfig.cassandra.cassPort)
      cassSettings = maybe basicCassSettings (\sslCtx -> Cassandra.setSSLContext sslCtx basicCassSettings) mbSSLContext
  cassClient <- Cassandra.init cassSettings
  let resources = backendResources (Map.elems intConfig.dynamicBackends)
  resourcePool <-
    liftIO $
      createBackendResourcePool
        resources
        intConfig.rabbitmq
        cassClient
  let sm =
        Map.fromList $
          [ (intConfig.backendOne.originDomain, intConfig.backendOne.beServiceMap),
            (intConfig.backendTwo.originDomain, intConfig.backendTwo.beServiceMap),
            (intConfig.federationV0.originDomain, intConfig.federationV0.beServiceMap)
          ]
            <> [(berDomain resource, resourceServiceMap resource) | resource <- resources]
  tempDir <- Codensity $ withSystemTempDirectory "test"
  timeOutSeconds <-
    liftIO $
      fromMaybe 10 . (readMaybe @Int =<<) <$> lookupEnv "TEST_TIMEOUT_SECONDS"
  pure
    GlobalEnv
      { gServiceMap = sm,
        gDomain1 = intConfig.backendOne.originDomain,
        gDomain2 = intConfig.backendTwo.originDomain,
        gFederationV0Domain = intConfig.federationV0.originDomain,
        gDynamicDomains = (.domain) <$> Map.elems intConfig.dynamicBackends,
        gDefaultAPIVersion = 6,
        gManager = manager,
        gServicesCwdBase = devEnvProjectRoot <&> (</> "services"),
        gRemovalKeyPath = error "Uninitialised removal key path",
        gBackendResourcePool = resourcePool,
        gRabbitMQConfig = intConfig.rabbitmq,
        gTempDir = tempDir,
        gTimeOutSeconds = timeOutSeconds
      }
  where
    createSSLContext :: Maybe FilePath -> IO (Maybe OpenSSL.SSLContext)
    createSSLContext (Just certFilePath) = do
      print ("TLS: Connecting to Cassandra with TLS. Provided CA path:" ++ certFilePath)
      sslContext <- OpenSSL.context
      OpenSSL.contextSetCAFile sslContext certFilePath
      OpenSSL.contextSetVerificationMode
        sslContext
        OpenSSL.VerifyPeer
          { vpFailIfNoPeerCert = True,
            vpClientOnce = True,
            vpCallback = Nothing
          }
      pure $ Just sslContext
    createSSLContext Nothing = pure Nothing

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
          federationV0Domain = gFederationV0Domain ge,
          dynamicDomains = gDynamicDomains ge,
          defaultAPIVersion = gDefaultAPIVersion ge,
          manager = gManager ge,
          servicesCwdBase = gServicesCwdBase ge,
          removalKeyPath = gRemovalKeyPath ge,
          prekeys = pks,
          lastPrekeys = lpks,
          mls = mls,
          resourcePool = ge.gBackendResourcePool,
          rabbitMQConfig = ge.gRabbitMQConfig,
          timeOutSeconds = ge.gTimeOutSeconds
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

emptyClientGroupState :: ClientGroupState
emptyClientGroupState = ClientGroupState Nothing Nothing

allCiphersuites :: [Ciphersuite]
allCiphersuites = map Ciphersuite ["0x0001", "0xf031"]

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
          epoch = 0,
          ciphersuite = def,
          protocol = MLSProtocolMLS
        }
