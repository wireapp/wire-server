{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Testlib.ModService where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Pool (withResource)
import Data.String.Conversions (cs)
import Data.Text hiding (elem, head)
import Data.Traversable
import Data.Word (Word16)
import qualified Data.Yaml as Yaml
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import qualified Network.Socket as N
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getEnv)
import System.FilePath
import System.IO
import qualified System.IO.Error as Error
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Posix (getEnvironment, killProcess, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Testlib.App
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.Types
import Text.RawString.QQ
import Prelude

withModifiedService ::
  Service ->
  -- | function that edits the config
  (Value -> App Value) ->
  -- | This action wil access the modified spawned service
  (String -> App a) ->
  App a
withModifiedService srv modConfig = withModifiedServices (Map.singleton srv modConfig)

copyDirectoryRecursively :: FilePath -> FilePath -> IO ()
copyDirectoryRecursively from to = do
  createDirectoryIfMissing True to
  files <- listDirectory from
  for_ files $ \file -> do
    let fromPath = from </> file
    let toPath = to </> file
    isDirectory <- doesDirectoryExist fromPath
    if isDirectory
      then copyDirectoryRecursively fromPath toPath
      else copyFile fromPath toPath

startDynamicBackend :: DynBackendConfigOverrides -> (String -> App a) -> App a
startDynamicBackend beOverrides action = do
  pool <- asks (.resourcePool)
  liftBaseWith $
    \runInBase ->
      withResource
        pool
        ( \resource -> runInBase $ do
            defDomain <- asks (.domain1)
            let services =
                  Map.mapWithKey
                    ( \srv conf ->
                        conf
                          >=> setKeyspace resource srv
                          >=> setEsIndex resource srv
                          >=> setFederatorPort resource srv
                    )
                    $ defaultDynBackendConfigOverridesToMap beOverrides
            startBackend
              resource.berDomain
              (Just $ setFederatorConfig resource)
              (Just $ backGroundWorkerOverrides resource)
              services
              ( \ports sm -> do
                  let templateBackend = fromMaybe (error "no default domain found in backends") $ sm & Map.lookup defDomain
                   in Map.insert resource.berDomain (setFederatorPorts resource $ updateServiceMap ports templateBackend) sm
              )
              action
        )
  where
    setFederatorConfig :: BackendResource -> Value -> App Value
    setFederatorConfig resource =
      setFieldIfExists "federatorInternal.port" resource.berFederatorInternal
        >=> setFieldIfExists "federatorExternal.port" resource.berFederatorExternal
        >=> setFieldIfExists "optSettings.setFederationDomain" resource.berDomain

    backGroundWorkerOverrides :: BackendResource -> Value -> App Value
    backGroundWorkerOverrides resource = setFieldIfExists "federatorInternal.port" resource.berFederatorInternal

    setFederatorPort :: BackendResource -> Service -> Value -> App Value
    setFederatorPort resource = \case
      Brig -> setFieldIfExists "federatorInternal.port" resource.berFederatorInternal
      Cargohold -> setFieldIfExists "federator.port" resource.berFederatorInternal
      Galley -> setFieldIfExists "federator.port" resource.berFederatorInternal
      _ -> pure

    setKeyspace :: BackendResource -> Service -> Value -> App Value
    setKeyspace resource = \case
      Galley -> setFieldIfExists "cassandra.keyspace" resource.berGalleyKeyspace
      Brig -> setFieldIfExists "cassandra.keyspace" resource.berBrigKeyspace
      Spar -> setFieldIfExists "cassandra.keyspace" resource.berSparKeyspace
      Gundeck -> setFieldIfExists "cassandra.keyspace" resource.berGundeckKeyspace
      -- other services do not have a DB
      _ -> pure

    setEsIndex :: BackendResource -> Service -> Value -> App Value
    setEsIndex resource = \case
      Brig -> setFieldIfExists "elasticsearch.index" resource.berElasticsearchIndex
      -- other services do not have an ES index
      _ -> pure

setFederatorPorts :: BackendResource -> ServiceMap -> ServiceMap
setFederatorPorts resource sm =
  sm
    { federatorInternal = sm.federatorInternal {host = "127.0.0.1", port = resource.berFederatorInternal},
      federatorExternal = sm.federatorExternal {host = "127.0.0.1", port = resource.berFederatorExternal}
    }

withModifiedServices :: Map.Map Service (Value -> App Value) -> (String -> App a) -> App a
withModifiedServices services action = do
  domain <- asks (.domain1)
  startBackend domain Nothing Nothing services (\ports -> Map.adjust (updateServiceMap ports) domain) action

updateServiceMap :: Map.Map Service Word16 -> ServiceMap -> ServiceMap
updateServiceMap ports serviceMap =
  Map.foldrWithKey
    ( \srv newPort sm ->
        case srv of
          Brig -> sm {brig = sm.brig {host = "127.0.0.1", port = newPort}}
          Galley -> sm {galley = sm.galley {host = "127.0.0.1", port = newPort}}
          Cannon -> sm {cannon = sm.cannon {host = "127.0.0.1", port = newPort}}
          Gundeck -> sm {gundeck = sm.gundeck {host = "127.0.0.1", port = newPort}}
          Cargohold -> sm {cargohold = sm.cargohold {host = "127.0.0.1", port = newPort}}
          Nginz -> sm {nginz = sm.nginz {host = "127.0.0.1", port = newPort}}
          Spar -> sm {spar = sm.spar {host = "127.0.0.1", port = newPort}}
    )
    serviceMap
    ports

startBackend ::
  String ->
  Maybe (Value -> App Value) ->
  Maybe (Value -> App Value) ->
  Map.Map Service (Value -> App Value) ->
  (Map.Map Service Word16 -> Map.Map String ServiceMap -> Map.Map String ServiceMap) ->
  (String -> App a) ->
  App a
startBackend domain mFederatorOverrides mBgWorkerOverrides services modifyBackends action = do
  ports <- Map.traverseWithKey (\_ _ -> liftIO openFreePort) services

  let updateServiceMapInConfig :: Maybe Service -> Value -> App Value
      updateServiceMapInConfig forSrv config =
        foldlM
          ( \c (srv, (port, _)) -> do
              overridden <-
                c
                  & setField
                    (serviceName srv)
                    ( object
                        ( [ "host" .= ("127.0.0.1" :: String),
                            "port" .= port
                          ]
                            <> (["externalHost" .= ("127.0.0.1" :: String) | srv == Cannon])
                        )
                    )
              case forSrv of
                Just Spar ->
                  overridden
                    -- FUTUREWORK: override "saml.spAppUri" and "saml.spSsoUri" with correct port, too?
                    & setField "saml.spHost" ("127.0.0.1" :: String)
                    & setField "saml.spPort" port
                Just Brig ->
                  overridden
                    & setField "optSettings.setFederationDomain" domain
                    & setField "optSettings.setFederationDomainConfigs" [object ["domain" .= domain, "search_policy" .= "full_search"]]
                Just Cargohold ->
                  overridden
                    & setField "settings.federationDomain" domain
                Just Galley ->
                  overridden
                    & setField "settings.federationDomain" domain
                    & setField "settings.featureFlags.classifiedDomains.config.domains" [domain]
                Just Gundeck ->
                  overridden
                    & setField "settings.federationDomain" domain
                _ -> pure overridden
          )
          config
          (Map.assocs ports)

  -- close all sockets before starting the services
  for_ (Map.keys services) $ \srv -> maybe (failApp "the impossible in withServices happened") (liftIO . N.close . snd) (Map.lookup srv ports)

  bgWorkerInstance <-
    case mBgWorkerOverrides of
      Nothing -> pure []
      Just override ->
        readServiceConfig' "background-worker"
          >>= override
          >>= startProcess "background-worker"
          <&> (: [])

  fedInstance <-
    case mFederatorOverrides of
      Nothing -> pure []
      Just override ->
        readServiceConfig' "federator"
          >>= updateServiceMapInConfig Nothing
          >>= override
          >>= startProcess "federator"
          <&> (: [])

  otherInstances <- for (Map.assocs services) $ \case
    (Nginz, _) -> do
      env <- ask
      sm <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup domain (modifyBackends (fromIntegral . fst <$> ports) env.serviceMap))
      port <- maybe (failApp "the impossible in withServices happened") (pure . fromIntegral . fst) (Map.lookup Nginz ports)
      startNginz port sm
    (srv, modifyConfig) -> do
      let srvName = serviceName srv
      readServiceConfig srv
        >>= updateServiceMapInConfig (Just srv)
        >>= modifyConfig
        >>= startProcess srvName

  let instances = bgWorkerInstance <> fedInstance <> otherInstances

  let stopInstances = liftIO $ do
        -- Running waitForProcess would hang for 30 seconds when the test suite
        -- is run from within ghci, so we don't wait here.
        for_ instances $ \(ph, path) -> do
          terminateProcess ph
          timeout 50000 (waitForProcess ph) >>= \case
            Just _ -> do
              putStrLn ("killing " <> path)
            Nothing -> do
              timeout 100000 (waitForProcess ph) >>= \case
                Just _ -> do
                  putStrLn ("killing again" <> path)
                Nothing -> do
                  putStrLn ("force-killing " <> path)
                  mPid <- getPid ph
                  for_ mPid (signalProcess killProcess)
                  void $ waitForProcess ph
          -- whenM (doesFileExist path) $ removeFile path
          whenM (doesDirectoryExist path) $ removeDirectoryRecursive path

  let modifyEnv env =
        env {serviceMap = modifyBackends (fromIntegral . fst <$> ports) env.serviceMap}

  let waitForAllServices = do
        env <- ask
        liftIO $
          mapConcurrently_
            (\srv -> runReaderT (unApp (waitUntilServiceUp domain srv)) env)
            (Map.keys ports)

  App $
    ReaderT
      ( \env ->
          runReaderT
            ( local
                modifyEnv
                ( unApp $ do
                    waitForAllServices
                    action domain
                )
            )
            env
            `finally` stopInstances
      )

startProcess :: String -> Value -> App (ProcessHandle, FilePath)
startProcess srvName config = do
  processEnv <- liftIO $ do
    environment <- getEnvironment
    rabbitMqUserName <- getEnv "RABBITMQ_USERNAME"
    rabbitMqPassword <- getEnv "RABBITMQ_PASSWORD"
    pure
      ( environment
          <> [ ("AWS_REGION", "eu-west-1"),
               ("AWS_ACCESS_KEY_ID", "dummykey"),
               ("AWS_SECRET_ACCESS_KEY", "dummysecret"),
               ("RABBITMQ_USERNAME", rabbitMqUserName),
               ("RABBITMQ_PASSWORD", rabbitMqPassword)
             ]
      )

  tempFile <- liftIO $ writeTempFile "/tmp" (srvName <> ".yaml") (cs $ Yaml.encode config)

  (cwd, exe) <-
    asks (.servicesCwdBase) <&> \case
      Nothing -> (Nothing, srvName)
      Just dir ->
        (Just (dir </> srvName), "../../dist" </> srvName)

  (_, _, _, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd, env = Just processEnv}
  pure (ph, tempFile)

waitUntilServiceUp :: HasCallStack => String -> Service -> App ()
waitUntilServiceUp domain = \case
  Nginz -> pure ()
  srv -> do
    isUp <-
      retrying
        (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
        (\_ isUp -> pure (not isUp))
        ( \_ -> do
            req <- baseRequest domain srv Unversioned "/i/status"
            env <- ask
            eith <-
              liftIO $
                E.try
                  ( runAppWithEnv env $ do
                      res <- submit "GET" req
                      pure (res.status `elem` [200, 204])
                  )
            pure $ either (\(_e :: HTTP.HttpException) -> False) id eith
        )
    unless isUp $
      failApp ("Time out for service " <> show srv <> " to come up")

-- | Open a TCP socket on a random free port. This is like 'warp''s
--   openFreePort.
--
--   Since 0.0.0.1
openFreePort :: IO (Int, N.Socket)
openFreePort =
  E.bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $
    \sock -> do
      N.bind sock $ N.SockAddrInet 0 $ N.tupleToHostAddress (127, 0, 0, 1)
      N.getSocketName sock >>= \case
        N.SockAddrInet port _ -> do
          pure (fromIntegral port, sock)
        addr ->
          E.throwIO $
            Error.mkIOError
              Error.userErrorType
              ( "openFreePort was unable to create socket with a SockAddrInet. "
                  <> "Got "
                  <> show addr
              )
              Nothing
              Nothing

startNginz :: Word16 -> ServiceMap -> App (ProcessHandle, FilePath)
startNginz port sm = do
  nginzConf <-
    liftIO $
      NginzConfig port
        <$> (openFreePort >>= \(p, s) -> N.close s $> fromIntegral p)
        <*> (openFreePort >>= \(p, s) -> N.close s $> fromIntegral p)
        <*> pure sm.federatorExternal.port

  -- Create a whole temporary directory and copy all nginx's config files.
  -- This is necessary because nginx assumes local imports are relative to
  -- the location of the main configuration file.
  (ph, tempFile) <- do
    tmpDir <- liftIO $ createTempDirectory "/tmp" "nginz"
    mBaseDir <- asks (.servicesCwdBase)
    basedir <- maybe (failApp "service cwd base not found") pure mBaseDir
    let srvName = serviceName Nginz

    -- copy all config files into the tmp dir
    liftIO $ do
      let from = basedir </> srvName </> "integration-test"
      copyDirectoryRecursively (from </> "conf" </> "nginz") (tmpDir </> "conf" </> "nginz")
      copyDirectoryRecursively (from </> "resources") (tmpDir </> "resources")

    -- override port configuration
    let portConfigTemplate =
          cs $
            [r|listen {port};
listen {http2_port} http2;
listen {ssl_port} ssl http2;
listen [::]:{ssl_port} ssl http2;
|]
    let portConfig =
          portConfigTemplate
            & replace (cs "{port}") (cs $ show nginzConf.localPort)
            & replace (cs "{http2_port}") (cs $ show nginzConf.http2Port)
            & replace (cs "{ssl_port}") (cs $ show nginzConf.sslPort)
    let integrationConfFile = tmpDir </> "conf" </> "nginz" </> "integration.conf"
    liftIO $ whenM (doesFileExist $ integrationConfFile) $ removeFile integrationConfFile
    liftIO $ writeFile integrationConfFile (cs portConfig)

    -- override upstreams
    let upstreamsCfg = tmpDir </> "conf" </> "nginz" </> "upstreams"
    liftIO $ whenM (doesFileExist $ upstreamsCfg) $ removeFile upstreamsCfg
    let upstreamTemplate =
          cs $
            [r|upstream {name} {
  least_conn;
  keepalive 32;
  server 127.0.0.1:{port} max_fails=3 weight=1;
}
|]

    forM_
      [ (serviceName Brig, sm.brig.port),
        (serviceName Cannon, sm.cannon.port),
        (serviceName Cargohold, sm.cargohold.port),
        (serviceName Galley, sm.galley.port),
        (serviceName Gundeck, sm.gundeck.port),
        (serviceName Nginz, sm.nginz.port),
        (serviceName Spar, sm.spar.port),
        ("proxy", sm.proxy.port)
      ]
      ( \case
          (srv, p) -> do
            let upstream =
                  upstreamTemplate
                    & replace (cs "{name}") (cs $ srv)
                    & replace (cs "{port}") (cs $ show p)
            liftIO $ appendFile upstreamsCfg (cs upstream)
      )
    let upstreamFederatorTemplate =
          cs $
            [r|upstream {name} {
  server 127.0.0.1:{port} max_fails=3 weight=1;
}|]
    liftIO $
      appendFile
        upstreamsCfg
        ( cs $
            upstreamFederatorTemplate
              & replace (cs "{name}") (cs "federator_external")
              & replace (cs "{port}") (cs $ show nginzConf.fedPort)
        )

    -- override pid configuration
    let pidConfigFile = tmpDir </> "conf" </> "nginz" </> "pid.conf"
    let pid = tmpDir </> "conf" </> "nginz" </> "nginz.pid"
    liftIO $ do
      whenM (doesFileExist $ pidConfigFile) $ removeFile pidConfigFile
      writeFile pidConfigFile (cs $ "pid " <> pid <> ";")

    -- start service
    (_, _, _, ph) <-
      liftIO $
        createProcess
          (proc "nginx" ["-c", tmpDir </> "conf" </> "nginz" </> "nginx.conf", "-p", tmpDir, "-g", "daemon off;"])
            { cwd = Just $ cs tmpDir
            }

    -- return handle and nginx tmp dir path
    pure (ph, tmpDir)

  pure (ph, tempFile)
