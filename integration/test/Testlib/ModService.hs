{-# LANGUAGE OverloadedStrings #-}

module Testlib.ModService
  ( withModifiedService,
    withModifiedServices,
    startDynamicBackend,
    startDynamicBackends,
    traverseConcurrentlyCodensity,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (finally)
import Control.Exception qualified as E
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Codensity
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import Data.Attoparsec.ByteString.Char8
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.String
import Data.String.Conversions (cs)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable
import Data.Word (Word16)
import Data.Yaml qualified as Yaml
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath
import System.IO
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Posix (killProcess, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Testlib.App
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.Printing
import Testlib.ResourcePool
import Testlib.Service
import Testlib.Types
import Text.RawString.QQ
import Prelude

withModifiedService ::
  Service ->
  -- | function that edits the config
  (Value -> App Value) ->
  (String -> App a) ->
  App a
withModifiedService srv modConfig = runCodensity $ withModifiedServices (Map.singleton srv modConfig)

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

-- | Concurrent traverse in the 'Codensity App' monad.
--
-- Every action is assumed to return an environment modification function. All
-- actions are started concurrently, and once they all yield control to their
-- continuation, the main continuation is run in an environment that
-- accumulates all the individual environment changes.
traverseConcurrentlyCodensity ::
  (HasCallStack => a -> Codensity App (Env -> Env)) ->
  (HasCallStack => [a] -> Codensity App (Env -> Env))
traverseConcurrentlyCodensity f args = do
  -- Create variables for synchronisation of the various threads:
  --  * @result@ is used to store the environment change, or possibly an exception
  --  * @done@ is used to signal that the main continuation has finished, so
  --    the thread can resume and move on to the cleanup phase.
  -- There is one pair of @(result, done)@ variables for each thread.
  vars <- liftIO $ for args $ \_ -> do
    result <- newEmptyMVar
    done <- newEmptyMVar
    pure (result, done)

  -- Create an IO Kleisli arrow that runs an action and synchronises using its
  -- two variables. This arrow will later be used to spawn a thread.
  runAction <- lift $ appToIOKleisli $ \((result, done), arg) ->
    catch
      ( runCodensity (f arg) $ \a -> liftIO $ do
          putMVar result (Right a)
          takeMVar done
      )
      $ \(e :: E.SomeException) ->
        void . liftIO $ tryPutMVar result (Left e)

  -- Spawn threads. Here we use the fact that 'withAsync' implicitly returns a
  -- 'Codensity' action, and use the 'Monad' instance of 'Codensity' to
  -- sequence these actions together. This is like nesting all the CPS
  -- invocations of 'withAsync' one inside the other, but without the need for
  -- explicit recursion.
  asyncs <- for (zip vars args) $ \x ->
    Codensity $ \k -> do
      k' <- appToIOKleisli k
      liftIO $ withAsync (runAction x) k'

  -- Wait for all the threads to return environment changes in their result
  -- variables. Any exception is rethrown here, and aborts the overall function.
  fs <- liftIO $ for vars $ \(result, _) ->
    takeMVar result >>= either throwM pure

  Codensity $ \k -> do
    -- Now compose all environment changes in some arbitrary order (they
    -- should mostly commute, anyway). Then apply these changes to the actual
    -- environment and also pass it to the main continuation @k@.
    let modifyEnv = appEndo (foldMap Endo fs)
    result <- local modifyEnv (k modifyEnv)

    -- Finally, signal all threads that it is time to clean up, and wait for
    -- them to finish. Note that this last block might not be executed in case
    -- of exceptions, but this is not a problem, because all the async threads
    -- are running within a 'withAsync' block, so they will be automatically
    -- cancelled in that case.
    liftIO $ traverse_ (\(_, d) -> putMVar d ()) vars
    liftIO $ traverse_ wait asyncs
    pure result

startDynamicBackends :: HasCallStack => [ServiceOverrides] -> (HasCallStack => [String] -> App a) -> App a
startDynamicBackends beOverrides k =
  runCodensity
    ( do
        when (Prelude.length beOverrides > 3) $ lift $ failApp "Too many backends. Currently only 3 are supported."
        pool <- asks (.resourcePool)
        resources <- acquireResources (Prelude.length beOverrides) pool
        void $ traverseConcurrentlyCodensity (uncurry startDynamicBackend) (zip resources beOverrides)
        pure $ map (.berDomain) resources
    )
    k

startDynamicBackend :: HasCallStack => BackendResource -> ServiceOverrides -> Codensity App (Env -> Env)
startDynamicBackend resource beOverrides = do
  let services =
        withOverrides beOverrides $
          Map.mapWithKey
            ( \srv conf ->
                conf
                  >=> setKeyspace srv
                  >=> setEsIndex srv
                  >=> setFederationSettings srv
                  >=> setAwsConfigs srv
                  >=> setLogLevel srv
            )
            defaultServiceOverridesToMap
  startBackend
    resource
    services
  where
    setAwsConfigs :: Service -> Value -> App Value
    setAwsConfigs = \case
      Brig ->
        setField "aws.userJournalQueue" resource.berAwsUserJournalQueue
          >=> setField "aws.prekeyTable" resource.berAwsPrekeyTable
          >=> setField "internalEvents.queueName" resource.berBrigInternalEvents
          >=> setField "emailSMS.email.sesQueue" resource.berEmailSMSSesQueue
          >=> setField "emailSMS.general.emailSender" resource.berEmailSMSEmailSender
      Cargohold -> setField "aws.s3Bucket" resource.berAwsS3Bucket
      Gundeck -> setField "aws.queueName" resource.berAwsQueueName
      Galley ->
        setField "journal.queueName" resource.berGalleyJournal
          >=> setField "rabbitmq.vHost" resource.berVHost
      BackgroundWorker -> setField "rabbitmq.vHost" resource.berVHost
      _ -> pure

    setFederationSettings :: Service -> Value -> App Value
    setFederationSettings =
      \case
        Brig ->
          setField "optSettings.setFederationDomain" resource.berDomain
            >=> setField "optSettings.setFederationDomainConfigs" ([] :: [Value])
            >=> setField "federatorInternal.port" resource.berFederatorInternal
            >=> setField "federatorInternal.host" ("127.0.0.1" :: String)
            >=> setField "rabbitmq.vHost" resource.berVHost
        Cargohold ->
          setField "settings.federationDomain" resource.berDomain
            >=> setField "federator.host" ("127.0.0.1" :: String)
            >=> setField "federator.port" resource.berFederatorInternal
        Galley ->
          setField "settings.federationDomain" resource.berDomain
            >=> setField "settings.featureFlags.classifiedDomains.config.domains" [resource.berDomain]
            >=> setField "federator.host" ("127.0.0.1" :: String)
            >=> setField "federator.port" resource.berFederatorInternal
            >=> setField "rabbitmq.vHost" resource.berVHost
        Gundeck -> setField "settings.federationDomain" resource.berDomain
        BackgroundWorker ->
          setField "federatorInternal.port" resource.berFederatorInternal
            >=> setField "federatorInternal.host" ("127.0.0.1" :: String)
            >=> setField "rabbitmq.vHost" resource.berVHost
        FederatorInternal ->
          setField "federatorInternal.port" resource.berFederatorInternal
            >=> setField "federatorExternal.port" resource.berFederatorExternal
            >=> setField "optSettings.setFederationDomain" resource.berDomain
        _ -> pure

    setKeyspace :: Service -> Value -> App Value
    setKeyspace = \case
      Galley -> setField "cassandra.keyspace" resource.berGalleyKeyspace
      Brig -> setField "cassandra.keyspace" resource.berBrigKeyspace
      Spar -> setField "cassandra.keyspace" resource.berSparKeyspace
      Gundeck -> setField "cassandra.keyspace" resource.berGundeckKeyspace
      -- other services do not have a DB
      _ -> pure

    setEsIndex :: Service -> Value -> App Value
    setEsIndex = \case
      Brig -> setField "elasticsearch.index" resource.berElasticsearchIndex
      -- other services do not have an ES index
      _ -> pure

    setLogLevel :: Service -> Value -> App Value
    setLogLevel = \case
      Spar -> setField "saml.logLevel" ("Warn" :: String)
      _ -> setField "logLevel" ("Warn" :: String)

serviceMapForResource :: BackendResource -> ServiceMap
serviceMapForResource resource =
  let g srv = HostPort "127.0.0.1" (berInternalServicePorts resource srv)
   in ServiceMap
        { brig = g Brig,
          backgroundWorker = g BackgroundWorker,
          cannon = g Cannon,
          cargohold = g Cargohold,
          federatorInternal = g FederatorInternal,
          federatorExternal = HostPort "127.0.0.1" resource.berFederatorExternal,
          galley = g Galley,
          gundeck = g Gundeck,
          nginz = g Nginz,
          spar = g Spar,
          proxy = HostPort "127.0.0.1" 6666,
          stern = g Stern
        }

withModifiedServices :: Map.Map Service (Value -> App Value) -> Codensity App String
withModifiedServices services = do
  pool <- asks (.resourcePool)
  [resource] <- acquireResources 1 pool
  void $
    startBackend
      resource
      services
  pure (resource.berDomain)

startBackend ::
  HasCallStack =>
  BackendResource ->
  Map.Map Service (Value -> App Value) ->
  Codensity App (Env -> Env)
startBackend resource services = do
  let domain = resource.berDomain
      staticPorts :: Map.Map Service Word16 = Map.fromList [(srv, berInternalServicePorts resource srv) | srv <- Map.keys services]

  let updateServiceMapInConfig :: Service -> Value -> App Value
      updateServiceMapInConfig forSrv config =
        foldlM
          ( \c (srv, port) -> do
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
              case (srv, forSrv) of
                (Spar, Spar) -> do
                  overridden
                    -- FUTUREWORK: override "saml.spAppUri" and "saml.spSsoUri" with correct port, too?
                    & setField "saml.spHost" ("127.0.0.1" :: String)
                    & setField "saml.spPort" port
                _ -> pure overridden
          )
          config
          (Map.assocs staticPorts)

  let updateServiceMap sm = Map.insert resource.berDomain (serviceMapForResource resource) sm

  stopInstances <- lift $ do
    instances <- for (Map.assocs services) $ \case
      (Nginz, _) -> do
        env <- ask
        sm <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup domain (updateServiceMap env.serviceMap))
        port <- maybe (failApp "the impossible in withServices happened") (pure . fromIntegral) (Map.lookup Nginz staticPorts)
        case env.servicesCwdBase of
          Nothing -> startNginzK8s domain sm
          Just _ -> startNginzLocal domain port resource.berNginzSslPort (Just resource.berNginzSslPort) sm
      (srv, modifyConfig) -> do
        readServiceConfig srv
          >>= updateServiceMapInConfig srv
          >>= modifyConfig
          >>= startProcess domain srv

    let stopInstances = liftIO $ do
          -- Running waitForProcess would hang for 30 seconds when the test suite
          -- is run from within ghci, so we don't wait here.
          for_ instances $ \(ph, path) -> do
            terminateProcess ph
            timeout 50000 (waitForProcess ph) >>= \case
              Just _ -> pure ()
              Nothing -> do
                timeout 100000 (waitForProcess ph) >>= \case
                  Just _ -> pure ()
                  Nothing -> do
                    mPid <- getPid ph
                    for_ mPid (signalProcess killProcess)
                    void $ waitForProcess ph
            whenM (doesFileExist path) $ removeFile path
            whenM (doesDirectoryExist path) $ removeDirectoryRecursive path

    pure stopInstances

  let modifyEnv env = env {serviceMap = updateServiceMap env.serviceMap}

  Codensity $ \action -> local modifyEnv $ do
    waitForService <- appToIOKleisli (waitUntilServiceUp domain)
    ioAction <- appToIO (action ())
    liftIO $
      (mapConcurrently_ waitForService (Map.keys services) >> ioAction)
        `finally` stopInstances

  pure modifyEnv

startProcess :: String -> Service -> Value -> App (ProcessHandle, FilePath)
startProcess domain srv = startProcess' domain (configName srv)

processColors :: [(String, String -> String)]
processColors =
  [ ("brig", colored green),
    ("galley", colored yellow),
    ("gundeck", colored blue),
    ("cannon", colored orange),
    ("cargohold", colored purpleish),
    ("spar", colored orange),
    ("federator", colored blue),
    ("background-worker", colored blue),
    ("nginx", colored purpleish)
  ]

startProcess' :: String -> String -> Value -> App (ProcessHandle, FilePath)
startProcess' domain execName config = do
  tempFile <- liftIO $ writeTempFile "/tmp" (execName <> "-" <> domain <> "-" <> ".yaml") (cs $ Yaml.encode config)

  (cwd, exe) <-
    asks (.servicesCwdBase) <&> \case
      Nothing -> (Nothing, execName)
      Just dir ->
        (Just (dir </> execName), "../../dist" </> execName)

  (_, Just stdoutHdl, Just stderrHdl, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd, std_out = CreatePipe, std_err = CreatePipe}
  let prefix = "[" <> execName <> "@" <> domain <> "] "
  let colorize = fromMaybe id (lookup execName processColors)
  void $ liftIO $ forkIO $ logToConsole colorize prefix stdoutHdl
  void $ liftIO $ forkIO $ logToConsole colorize prefix stderrHdl
  pure (ph, tempFile)

logToConsole :: (String -> String) -> String -> Handle -> IO ()
logToConsole colorize prefix hdl = do
  let go =
        ( do
            line <- hGetLine hdl
            putStrLn (colorize (prefix <> line))
            go
        )
          `E.catch` (\(_ :: E.IOException) -> pure ())
  go

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
            checkStatus <- appToIO $ do
              res <- submit "GET" req
              pure (res.status `elem` [200, 204])
            eith <- liftIO (E.try checkStatus)
            pure $ either (\(_e :: HTTP.HttpException) -> False) id eith
        )
    unless isUp $
      failApp ("Time out for service " <> show srv <> " to come up")

startNginzK8s :: String -> ServiceMap -> App (ProcessHandle, FilePath)
startNginzK8s domain sm = do
  tmpDir <- liftIO $ createTempDirectory "/tmp" ("nginz" <> "-" <> domain)
  liftIO $
    copyDirectoryRecursively "/etc/wire/nginz/" tmpDir

  let nginxConfFile = tmpDir </> "conf" </> "nginx.conf"
      upstreamsCfg = tmpDir </> "upstreams.conf"
  liftIO $ do
    conf <- Text.readFile nginxConfFile
    Text.writeFile nginxConfFile $
      ( conf
          & Text.replace "access_log /dev/stdout" "access_log /dev/null"
          -- TODO: Get these ports out of config
          & Text.replace ("listen 8080;\n    listen 8081 proxy_protocol;") (cs $ "listen " <> show sm.nginz.port <> ";")
          & Text.replace ("listen 8082;") (cs $ "listen unix:" <> (tmpDir </> "metrics-socket") <> ";")
          & Text.replace ("/var/run/nginz.pid") (cs $ tmpDir </> "nginz.pid")
          & Text.replace ("/etc/wire/nginz/upstreams/upstreams.conf") (cs upstreamsCfg)
      )
  createUpstreamsCfg upstreamsCfg sm
  ph <- startNginz domain nginxConfFile "/"
  pure (ph, tmpDir)

startNginzLocal :: String -> Word16 -> Word16 -> Maybe Word16 -> ServiceMap -> App (ProcessHandle, FilePath)
startNginzLocal domain localPort http2Port mSslPort sm = do
  -- Create a whole temporary directory and copy all nginx's config files.
  -- This is necessary because nginx assumes local imports are relative to
  -- the location of the main configuration file.
  tmpDir <- liftIO $ createTempDirectory "/tmp" ("nginz" <> "-" <> domain)
  mBaseDir <- asks (.servicesCwdBase)
  basedir <- maybe (failApp "service cwd base not found") pure mBaseDir

  -- copy all config files into the tmp dir
  liftIO $ do
    let from = basedir </> "nginz" </> "integration-test"
    copyDirectoryRecursively (from </> "conf" </> "nginz") (tmpDir </> "conf" </> "nginz")
    copyDirectoryRecursively (from </> "resources") (tmpDir </> "resources")

  let integrationConfFile = tmpDir </> "conf" </> "nginz" </> "integration.conf"

  -- hide access log
  let nginxConfFile = tmpDir </> "conf" </> "nginz" </> "nginx.conf"
  liftIO $ do
    conf <- Text.readFile nginxConfFile
    Text.writeFile nginxConfFile $
      ( conf
          & Text.replace "access_log /dev/stdout" "access_log /dev/null"
      )

  conf <- Prelude.lines <$> liftIO (readFile integrationConfFile)
  let sslPortParser = do
        _ <- string "listen"
        _ <- many1 space
        p <- many1 digit
        _ <- many1 space
        _ <- string "ssl"
        _ <- many1 space
        _ <- string "http2"
        _ <- many1 space
        _ <- char ';'
        pure (read p :: Word16)

  let mParsedPort =
        mapMaybe (eitherToMaybe . parseOnly sslPortParser . cs) conf
          & (\case [] -> Nothing; (p : _) -> Just p)

  sslPort <- maybe (failApp "could not determine nginz's ssl port") pure (mSslPort <|> mParsedPort)

  -- override port configuration
  let portConfigTemplate =
        [r|listen {localPort};
listen {ssl_port} ssl http2;
listen [::]:{ssl_port} ssl http2;
|]
  let portConfig =
        portConfigTemplate
          & Text.replace "{localPort}" (cs $ show localPort)
          & Text.replace "{http2_port}" (cs $ show http2Port)
          & Text.replace "{ssl_port}" (cs $ show sslPort)

  liftIO $ whenM (doesFileExist integrationConfFile) $ removeFile integrationConfFile
  liftIO $ writeFile integrationConfFile (cs portConfig)

  -- override upstreams
  let upstreamsCfg = tmpDir </> "conf" </> "nginz" </> "upstreams"
  createUpstreamsCfg upstreamsCfg sm
  let upstreamFederatorTemplate =
        [r|upstream {name} {
server 127.0.0.1:{port} max_fails=3 weight=1;
}|]
  liftIO $
    appendFile
      upstreamsCfg
      ( cs $
          upstreamFederatorTemplate
            & Text.replace "{name}" "federator_external"
            & Text.replace "{port}" (cs $ show sm.federatorExternal.port)
      )

  -- override pid configuration
  let pidConfigFile = tmpDir </> "conf" </> "nginz" </> "pid.conf"
  let pid = tmpDir </> "conf" </> "nginz" </> "nginz.pid"
  liftIO $ do
    whenM (doesFileExist $ pidConfigFile) $ removeFile pidConfigFile
    writeFile pidConfigFile (cs $ "pid " <> pid <> ";")

  -- start service
  ph <- startNginz domain nginxConfFile tmpDir

  -- return handle and nginx tmp dir path
  pure (ph, tmpDir)

createUpstreamsCfg :: String -> ServiceMap -> App ()
createUpstreamsCfg upstreamsCfg sm = do
  liftIO $ whenM (doesFileExist upstreamsCfg) $ removeFile upstreamsCfg
  let upstreamTemplate =
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
                  & Text.replace "{name}" (cs $ srv)
                  & Text.replace "{port}" (cs $ show p)
          liftIO $ appendFile upstreamsCfg (cs upstream)
    )

startNginz :: String -> FilePath -> FilePath -> App ProcessHandle
startNginz domain conf workingDir = do
  (_, Just stdoutHdl, Just stderrHdl, ph) <-
    liftIO $
      createProcess
        (proc "nginx" ["-c", conf, "-g", "daemon off;", "-e", "/dev/stdout"])
          { cwd = Just workingDir,
            std_out = CreatePipe,
            std_err = CreatePipe
          }

  let prefix = "[" <> "nginz" <> "@" <> domain <> "] "
  let colorize = fromMaybe id (lookup "nginx" processColors)
  void $ liftIO $ forkIO $ logToConsole colorize prefix stdoutHdl
  void $ liftIO $ forkIO $ logToConsole colorize prefix stderrHdl

  -- return handle and nginx tmp dir path
  pure ph
