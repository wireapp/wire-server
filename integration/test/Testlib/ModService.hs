{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Testlib.ModService
  ( withModifiedService,
    withModifiedServices,
    startOneDynBackend,
    startTwoDynBackends,
    startThreeDynBackends,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import Data.Attoparsec.ByteString.Char8
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String.Conversions (cs)
import Data.Text hiding (elem, head, zip)
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
import Testlib.ResourcePool
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

startOneDynBackend :: DynBackendConfigOverrides -> (String -> App ()) -> App ()
startOneDynBackend o a = startDynamicBackends [o] $ \case
  [d] -> a d
  _ -> assertFailure "Expected one dynamic backend to be started, but got more or less"

startTwoDynBackends :: DynBackendConfigOverrides -> DynBackendConfigOverrides -> (String -> String -> App ()) -> App ()
startTwoDynBackends o1 o2 a = startDynamicBackends [o1, o2] $ \case
  [d1, d2] -> a d1 d2
  _ -> assertFailure "Expected two dynamic backends to be started, but got more or less"

startThreeDynBackends :: DynBackendConfigOverrides -> DynBackendConfigOverrides -> DynBackendConfigOverrides -> (String -> String -> String -> App ()) -> App ()
startThreeDynBackends o1 o2 o3 a = startDynamicBackends [o1, o2, o3] $ \case
  [d1, d2, d3] -> a d1 d2 d3
  _ -> assertFailure "Expected three dynamic backends to be started, but got more or less"

startDynamicBackends :: [DynBackendConfigOverrides] -> ([String] -> App ()) -> App ()
startDynamicBackends beOverrides action = do
  pool <- asks (.resourcePool)
  withResources (Prelude.length beOverrides) pool $ \resources -> do
    let recStartBackends :: [String] -> [(BackendResource, DynBackendConfigOverrides)] -> App ()
        recStartBackends domains = \case
          [] -> action domains
          (res, o) : xs -> startDynamicBackend res o (\d -> recStartBackends (d : domains) xs)
    recStartBackends [] (zip resources beOverrides)

startDynamicBackend :: BackendResource -> DynBackendConfigOverrides -> (String -> App a) -> App a
startDynamicBackend resource beOverrides action = do
  defDomain <- asks (.domain1)
  defDomain2 <- asks (.domain2)
  let services =
        Map.mapWithKey
          ( \srv conf ->
              conf
                >=> setKeyspace srv
                >=> setEsIndex srv
                >=> setFederationSettings defDomain defDomain2 srv
                >=> setAwsAdnQueuesConfigs srv
          )
          $ defaultDynBackendConfigOverridesToMap beOverrides
  startBackend
    resource.berDomain
    (Just resource.berNginzSslPort)
    (Just setFederatorConfig)
    (Just $ backGroundWorkerOverrides (remoteDomains resource.berDomain))
    services
    ( \ports sm -> do
        let templateBackend = fromMaybe (error "no default domain found in backends") $ sm & Map.lookup defDomain
         in Map.insert resource.berDomain (setFederatorPorts resource $ updateServiceMap ports templateBackend) sm
    )
    (action resource.berDomain)
  where
    setAwsAdnQueuesConfigs :: Service -> Value -> App Value
    setAwsAdnQueuesConfigs = \case
      Brig ->
        setFieldIfExists "aws.userJournalQueue" resource.berAwsUserJournalQueue
          >=> setFieldIfExists "aws.prekeyTable" resource.berAwsPrekeyTable
          >=> setFieldIfExists "internalEvents.queueName" resource.berBrigInternalEvents
          >=> setFieldIfExists "emailSMS.email.sesQueue" resource.berEmailSMSSesQueue
          >=> setFieldIfExists "emailSMS.general.emailSender" resource.berEmailSMSEmailSender
          >=> setField "rabbitmq.vHost" resource.berVHost
      Cargohold -> setFieldIfExists "aws.s3Bucket" resource.berAwsS3Bucket
      Gundeck -> setFieldIfExists "aws.queueName" resource.berAwsQueueName
      Galley ->
        setFieldIfExists "journal.queueName" resource.berGalleyJournal
          >=> setField "rabbitmq.vHost" resource.berVHost
      _ -> pure

    setFederationSettings :: String -> String -> Service -> Value -> App Value
    setFederationSettings ownDomain otherDomain =
      \case
        Brig ->
          setField "optSettings.setFederationDomain" resource.berDomain
            >=> setField
              "optSettings.setFederationDomainConfigs"
              ( [ object ["domain" .= resource.berDomain, "search_policy" .= "full_search"],
                  object ["domain" .= ownDomain, "search_policy" .= "full_search"],
                  object ["domain" .= otherDomain, "search_policy" .= "full_search"]
                ]
                  <> [object ["domain" .= d, "search_policy" .= "full_search"] | d <- remoteDomains resource.berDomain]
              )
            >=> setFieldIfExists "federatorInternal.port" resource.berFederatorInternal
        Cargohold ->
          setField "settings.federationDomain" resource.berDomain
            >=> setFieldIfExists "federator.port" resource.berFederatorInternal
        Galley ->
          setField "settings.federationDomain" resource.berDomain
            >=> setField "settings.featureFlags.classifiedDomains.config.domains" [resource.berDomain]
            >=> setFieldIfExists "federator.port" resource.berFederatorInternal
        Gundeck -> setField "settings.federationDomain" resource.berDomain
        _ -> pure

    setFederatorConfig :: Value -> App Value
    setFederatorConfig =
      setFieldIfExists "federatorInternal.port" resource.berFederatorInternal
        >=> setFieldIfExists "federatorExternal.port" resource.berFederatorExternal
        >=> setFieldIfExists "optSettings.setFederationDomain" resource.berDomain

    backGroundWorkerOverrides :: [String] -> Value -> App Value
    backGroundWorkerOverrides rds =
      setFieldIfExists "federatorInternal.port" resource.berFederatorInternal
        >=> setField "remoteDomains" rds
        >=> setField "rabbitmq.vHost" resource.berVHost

    setKeyspace :: Service -> Value -> App Value
    setKeyspace = \case
      Galley -> setFieldIfExists "cassandra.keyspace" resource.berGalleyKeyspace
      Brig -> setFieldIfExists "cassandra.keyspace" resource.berBrigKeyspace
      Spar -> setFieldIfExists "cassandra.keyspace" resource.berSparKeyspace
      Gundeck -> setFieldIfExists "cassandra.keyspace" resource.berGundeckKeyspace
      -- other services do not have a DB
      _ -> pure

    setEsIndex :: Service -> Value -> App Value
    setEsIndex = \case
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
  startBackend domain Nothing Nothing Nothing services (\ports -> Map.adjust (updateServiceMap ports) domain) (action domain)

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
  Maybe Word16 ->
  Maybe (Value -> App Value) ->
  Maybe (Value -> App Value) ->
  Map.Map Service (Value -> App Value) ->
  (Map.Map Service Word16 -> Map.Map String ServiceMap -> Map.Map String ServiceMap) ->
  App a ->
  App a
startBackend domain nginzSslPort mFederatorOverrides mBgWorkerOverrides services modifyBackends action = do
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
          >>= startProcess domain "background-worker"
          <&> (: [])

  fedInstance <-
    case mFederatorOverrides of
      Nothing -> pure []
      Just override ->
        readServiceConfig' "federator"
          >>= updateServiceMapInConfig Nothing
          >>= override
          >>= startProcess domain "federator"
          <&> (: [])

  otherInstances <- for (Map.assocs services) $ \case
    (Nginz, _) -> do
      env <- ask
      sm <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup domain (modifyBackends (fromIntegral . fst <$> ports) env.serviceMap))
      port <- maybe (failApp "the impossible in withServices happened") (pure . fromIntegral . fst) (Map.lookup Nginz ports)
      startNginz domain port nginzSslPort sm
    (srv, modifyConfig) -> do
      let srvName = serviceName srv
      readServiceConfig srv
        >>= updateServiceMapInConfig (Just srv)
        >>= modifyConfig
        >>= startProcess domain srvName

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
          whenM (doesFileExist path) $ removeFile path
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
                    action
                )
            )
            env
            `finally` stopInstances
      )

startProcess :: String -> String -> Value -> App (ProcessHandle, FilePath)
startProcess domain srvName config = do
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

  tempFile <- liftIO $ writeTempFile "/tmp" (srvName <> "-" <> domain <> "-" <> ".yaml") (cs $ Yaml.encode config)

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

startNginz :: String -> Word16 -> Maybe Word16 -> ServiceMap -> App (ProcessHandle, FilePath)
startNginz domain port mSslPort sm = do
  -- Create a whole temporary directory and copy all nginx's config files.
  -- This is necessary because nginx assumes local imports are relative to
  -- the location of the main configuration file.
  tmpDir <- liftIO $ createTempDirectory "/tmp" ("nginz" <> "-" <> domain)
  mBaseDir <- asks (.servicesCwdBase)
  basedir <- maybe (failApp "service cwd base not found") pure mBaseDir
  let srvName = serviceName Nginz

  -- copy all config files into the tmp dir
  liftIO $ do
    let from = basedir </> srvName </> "integration-test"
    copyDirectoryRecursively (from </> "conf" </> "nginz") (tmpDir </> "conf" </> "nginz")
    copyDirectoryRecursively (from </> "resources") (tmpDir </> "resources")

  let integrationConfFile = tmpDir </> "conf" </> "nginz" </> "integration.conf"
  conf <- Prelude.lines <$> liftIO (readFile integrationConfFile)
  let sslPortParser = do
        _ <- string (cs "listen")
        _ <- many1 space
        p <- many1 digit
        _ <- many1 space
        _ <- string (cs "ssl")
        _ <- many1 space
        _ <- string (cs "http2")
        _ <- many1 space
        _ <- char ';'
        pure (read p :: Word16)

  let mParsedPort =
        mapMaybe (eitherToMaybe . parseOnly sslPortParser . cs) conf
          & (\case [] -> Nothing; (p : _) -> Just p)

  sslPort <- maybe (failApp "could not determine nginz's ssl port") pure (mSslPort <|> mParsedPort)

  nginzConf <-
    liftIO $
      NginzConfig port
        <$> (openFreePort >>= \(p, s) -> N.close s $> fromIntegral p)
        <*> pure sslPort
        <*> pure sm.federatorExternal.port

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
  liftIO $ whenM (doesFileExist integrationConfFile) $ removeFile integrationConfFile
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
