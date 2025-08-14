{-# LANGUAGE OverloadedStrings #-}

module Testlib.ModService
  ( withModifiedBackend,
    startDynamicBackend,
    startDynamicBackends,
    startDynamicBackendsReturnResources,
    traverseConcurrentlyCodensity,
    logToConsole,
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Codensity
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import qualified Data.Attoparsec.Text as Parser
import qualified Data.Char as Char
import Data.Default
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.String.Conversions (cs)
import Data.String.Interpolate
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable
import Data.Word
import qualified Data.Yaml as Yaml
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (createTempDirectory, writeTempFile)
import qualified System.Linux.Proc as LinuxProc
import System.Posix (keyboardSignal, killProcess, signalProcess)
import System.Posix.Types
import System.Process
import Testlib.App
import Testlib.HTTP
import Testlib.JSON
import Testlib.Ports (PortNamespace (..))
import Testlib.Printing
import Testlib.ResourcePool
import Testlib.Types
import Text.RE.Replace
import Text.RE.TDFA
import qualified UnliftIO
import Prelude

withModifiedBackend :: (HasCallStack) => ServiceOverrides -> ((HasCallStack) => String -> App a) -> App a
withModifiedBackend overrides k =
  startDynamicBackends [overrides] (\[domains] -> k domains)

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
traverseConcurrentlyCodensity ::
  ((HasCallStack) => a -> Codensity App ()) ->
  ((HasCallStack) => [a] -> Codensity App ())
traverseConcurrentlyCodensity f args = do
  -- Create variables for synchronisation of the various threads:
  --  * @result@ is used to store a possible exception
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
      ( runCodensity (f arg) $ \_ -> liftIO $ do
          putMVar result Nothing
          takeMVar done
      )
      $ \(e :: E.SomeException) ->
        void . liftIO $ tryPutMVar result (Just e)

  -- Spawn threads. Here we use the fact that 'withAsync' implicitly returns a
  -- 'Codensity' action, and use the 'Monad' instance of 'Codensity' to
  -- sequence these actions together. This is like nesting all the CPS
  -- invocations of 'withAsync' one inside the other, but without the need for
  -- explicit recursion.
  asyncs <- for (zip vars args) $ \x ->
    hoistCodensity $ Codensity $ withAsync (runAction x)

  -- Wait for all the threads set their result variables. Any exception is
  -- rethrown here, and aborts the overall function.
  liftIO $ for_ vars $ \(result, _) ->
    takeMVar result >>= maybe (pure ()) throwM

  Codensity $ \k -> do
    -- Now run the main continuation.
    result <- k ()

    -- Finally, signal all threads that it is time to clean up, and wait for
    -- them to finish. Note that this last block might not be executed in case
    -- of exceptions, but this is not a problem, because all the async threads
    -- are running within a 'withAsync' block, so they will be automatically
    -- cancelled in that case.
    liftIO $ traverse_ (\(_, d) -> putMVar d ()) vars
    liftIO $ traverse_ wait asyncs

    pure result

startDynamicBackends :: (HasCallStack) => [ServiceOverrides] -> ([String] -> App a) -> App a
startDynamicBackends beOverrides k = do
  startDynamicBackendsReturnResources beOverrides (\resources -> k $ map (.berDomain) resources)

startDynamicBackendsReturnResources :: (HasCallStack) => [ServiceOverrides] -> ([BackendResource] -> App a) -> App a
startDynamicBackendsReturnResources beOverrides k = do
  let startDynamicBackendsCodensity = do
        when (Prelude.length beOverrides > 3) $ lift $ failApp "Too many backends. Currently only 3 are supported."
        pool <- asks (.resourcePool)
        resources <- acquireResources (Prelude.length beOverrides) pool
        void $
          traverseConcurrentlyCodensity
            (void . uncurry startDynamicBackend)
            (zip resources beOverrides)
        pure resources
  runCodensity startDynamicBackendsCodensity k

startDynamicBackend :: (HasCallStack) => BackendResource -> ServiceOverrides -> Codensity App String
startDynamicBackend resource beOverrides = do
  let overrides =
        mconcat
          [ setKeyspace,
            setEsIndex,
            setPgDb,
            setFederationSettings,
            setAwsConfigs,
            setMlsPrivateKeyPaths,
            setLogLevel,
            beOverrides
          ]
  startBackend resource overrides
  pure resource.berDomain
  where
    setAwsConfigs :: ServiceOverrides
    setAwsConfigs =
      def
        { brigCfg =
            setField "aws.userJournalQueue" resource.berAwsUserJournalQueue
              >=> setField "aws.prekeyTable" resource.berAwsPrekeyTable
              >=> setField "internalEvents.queueName" resource.berBrigInternalEvents
              >=> setField "emailSMS.email.sesQueue" resource.berEmailSMSSesQueue
              >=> setField "emailSMS.general.emailSender" resource.berEmailSMSEmailSender,
          cargoholdCfg = setField "aws.s3Bucket" resource.berAwsS3Bucket,
          gundeckCfg = setField "aws.queueName" resource.berAwsQueueName,
          galleyCfg = setField "journal.queueName" resource.berGalleyJournal
        }

    setFederationSettings :: ServiceOverrides
    setFederationSettings =
      def
        { brigCfg =
            setField "optSettings.setFederationDomain" resource.berDomain
              >=> setField "optSettings.setFederationDomainConfigs" ([] :: [Value])
              >=> setField "federatorInternal.port" resource.berFederatorInternal
              >=> setField "federatorInternal.host" ("127.0.0.1" :: String)
              >=> setField "rabbitmq.vHost" resource.berVHost,
          cargoholdCfg =
            setField "settings.federationDomain" resource.berDomain
              >=> setField "federator.host" ("127.0.0.1" :: String)
              >=> setField "federator.port" resource.berFederatorInternal,
          galleyCfg =
            setField "settings.federationDomain" resource.berDomain
              >=> setField "settings.featureFlags.classifiedDomains.config.domains" [resource.berDomain]
              >=> setField "federator.host" ("127.0.0.1" :: String)
              >=> setField "federator.port" resource.berFederatorInternal
              >=> setField "rabbitmq.vHost" resource.berVHost,
          gundeckCfg =
            setField "settings.federationDomain" resource.berDomain
              >=> setField "rabbitmq.vHost" resource.berVHost,
          backgroundWorkerCfg =
            setField "federatorInternal.port" resource.berFederatorInternal
              >=> setField "federatorInternal.host" ("127.0.0.1" :: String)
              >=> setField "rabbitmq.vHost" resource.berVHost,
          federatorInternalCfg =
            setField "federatorInternal.port" resource.berFederatorInternal
              >=> setField "federatorExternal.port" resource.berFederatorExternal
              >=> setField "optSettings.setFederationDomain" resource.berDomain,
          cannonCfg =
            setField "rabbitmq.vHost" resource.berVHost
        }

    setKeyspace :: ServiceOverrides
    setKeyspace =
      def
        { galleyCfg = setField "cassandra.keyspace" resource.berGalleyKeyspace,
          brigCfg = setField "cassandra.keyspace" resource.berBrigKeyspace,
          sparCfg = setField "cassandra.keyspace" resource.berSparKeyspace,
          gundeckCfg = setField "cassandra.keyspace" resource.berGundeckKeyspace
        }
    setPgDb :: ServiceOverrides
    setPgDb =
      def
        { brigCfg = setField "postgresql.dbname" resource.berPostgresqlDBName,
          galleyCfg = setField "postgresql.dbname" resource.berPostgresqlDBName
        }

    setEsIndex :: ServiceOverrides
    setEsIndex =
      def
        { brigCfg = setField "elasticsearch.index" resource.berElasticsearchIndex
        }

    setMlsPrivateKeyPaths :: ServiceOverrides
    setMlsPrivateKeyPaths =
      def
        { galleyCfg = setField "settings.mlsPrivateKeyPaths" resource.berMlsPrivateKeyPaths
        }

    setLogLevel :: ServiceOverrides
    setLogLevel =
      def
        { sparCfg = setField "saml.logLevel" ("Warn" :: String),
          brigCfg = setField "logLevel" ("Warn" :: String),
          cannonCfg = setField "logLevel" ("Warn" :: String),
          cargoholdCfg = setField "logLevel" ("Warn" :: String),
          galleyCfg = setField "logLevel" ("Warn" :: String),
          gundeckCfg = setField "logLevel" ("Warn" :: String),
          nginzCfg = setField "logLevel" ("Warn" :: String),
          backgroundWorkerCfg = setField "logLevel" ("Warn" :: String),
          sternCfg = setField "logLevel" ("Warn" :: String),
          federatorInternalCfg = setField "logLevel" ("Warn" :: String),
          wireProxyCfg = setField "logLevel" ("Warn" :: String)
        }

updateServiceMapInConfig :: BackendResource -> Service -> Value -> App Value
updateServiceMapInConfig resource forSrv config =
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
    [ (srv, berInternalServicePorts resource srv :: Int)
      | srv <- allServices,
        -- if a service is not enabled, do not set its endpoint configuration,
        -- unless we are starting the service itself
        berEnableService resource srv || srv == forSrv
    ]

startBackend ::
  (HasCallStack) =>
  BackendResource ->
  ServiceOverrides ->
  Codensity App ()
startBackend resource overrides = do
  lift $ waitForPortsToBeFree resource
  traverseConcurrentlyCodensity (withProcess resource overrides) allServices
  lift $ ensureBackendReachable resource.berDomain

waitForPortsToBeFree :: (HasCallStack) => BackendResource -> App ()
waitForPortsToBeFree backend = do
  let namedPorts =
        (FederatorExternal, backend.berFederatorExternal)
          : (NginzHttp2, backend.berNginzHttp2Port)
          : (NginzSSL, backend.berNginzSslPort)
          : map (\s -> (ServiceInternal s, berInternalServicePorts backend s)) [minBound .. maxBound]
  void $ UnliftIO.pooledMapConcurrentlyN 8 (uncurry $ waitForPortToBeFree backend.berDomain) namedPorts

-- | Using lsof because it is most convenient. Checking if a port is free in Haskell involves binding to it which is not what we want.
waitForPortToBeFree :: String -> PortNamespace -> Word16 -> App ()
waitForPortToBeFree domain portName portNumber = do
  env <- ask
  addFailureContext ("domain=" <> domain <> "\nportName=" <> show portName <> "\nportNumber=" <> show portNumber) $
    UnliftIO.timeout (env.timeOutSeconds * 1_000_000) check >>= \case
      Nothing -> assertFailure $ "timeout waiting for federator port to be free: name=" <> show portName <> ", number=" <> show portNumber
      Just _ -> pure ()
  where
    check :: App ()
    check = do
      env <- ask
      let process = (proc "lsof" ["-Q", "-Fpc", "-i", ":" <> show portNumber, "-s", "TCP:LISTEN"]) {std_out = CreatePipe, std_err = CreatePipe}
      (_, Just stdoutHdl, Just stderrHdl, ph) <- liftIO $ createProcess process
      let prefix = "[" <> "lsof(" <> show portName <> ")@" <> domain <> maybe "" (":" <>) env.currentTestName <> "] "
      liftIO $ void $ forkIO $ logToConsole id prefix stderrHdl
      exitCode <- liftIO $ waitForProcess ph
      case exitCode of
        ExitFailure _ -> assertFailure $ prefix <> "lsof failed to figure out if port is free"
        ExitSuccess -> do
          lsofOutput <- liftIO $ hGetContents stdoutHdl
          case parseLsof (fromString lsofOutput) of
            Right procs@(_ : _) -> do
              liftIO $ putStrLn $ colored red $ prefix <> "Found one or more processes listening on port: " <> show portNumber
              analysis <- List.intercalate "\n" <$> mapM (liftIO . uncurry analyzeRunningProcess) procs
              liftIO $ putStrLn $ indent 2 analysis
              liftIO $ threadDelay 100_000
              check
            Right [] -> pure ()
            Left e -> assertFailure $ "Failed while parsing lsof output with error: " <> e <> "\n" <> "lsof output:\n" <> lsofOutput

analyzeRunningProcess :: ProcessID -> String -> IO String
analyzeRunningProcess pid pname = do
  eithSocket <- LinuxProc.readProcTcpSockets (LinuxProc.ProcessId $ fromIntegral pid)
  let sockInfo = case eithSocket of
        Left e -> "Failed to read TCP sockets for process: error: " <> Text.unpack (LinuxProc.renderProcError e)
        Right socks -> List.intercalate "\n" $ map displaySocket socks
  pure $ "Process: pid=" <> show pid <> ", name=" <> pname <> "\n" <> indent 2 sockInfo
  where
    displaySocket :: LinuxProc.TcpSocket -> String
    displaySocket sock = "local address = " <> show sock.tcpLocalAddr <> ", remote address = " <> show sock.tcpRemoteAddr <> ", tcp state = " <> show sock.tcpTcpState

-- | Example lsof output:
--
-- @
-- p61317
-- cfederator
--
-- @
parseLsof :: String -> Either String [(ProcessID, String)]
parseLsof output =
  Parser.parseOnly (listParser <* trailingSpace <* Parser.endOfInput) (fromString output)
  where
    lsofParser :: Parser.Parser (ProcessID, String)
    lsofParser =
      (,) <$> processIdParser <* Parser.char '\n' <*> processNameParser
    processIdParser = Parser.char 'p' *> Parser.decimal
    processNameParser = Parser.char 'c' *> Parser.many1 (Parser.satisfy (/= '\n'))

    listParser = (Parser.sepBy lsofParser (Parser.char '\n'))
    trailingSpace = Parser.many' (Parser.satisfy Char.isSpace)

ensureBackendReachable :: (HasCallStack) => String -> App ()
ensureBackendReachable domain = do
  env <- ask
  let checkServiceIsUpReq :: (HasCallStack) => App Bool
      checkServiceIsUpReq = do
        req <-
          rawBaseRequest
            env.domain1
            FederatorInternal
            Unversioned
            ("/rpc/" <> domain <> "/brig/api-version")
            <&> (addHeader "Wire-Origin-Domain" env.domain1)
              . (addJSONObject [])
        checkStatus <- appToIO $ do
          res <- submit "POST" req

          -- If we get 533 here it means federation is not available between domains
          -- but ingress is working, since we're processing the request.
          let is200 = res.status == 200
          mInner <- lookupField res.json "inner"
          isFedDenied <- case mInner of
            Nothing -> pure False
            Just inner -> do
              label <- inner %. "label" & asString
              pure $ res.status == 533 && label == "federation-denied"

          pure (is200 || isFedDenied)
        eith <- liftIO (E.try checkStatus)
        pure $ either (\(_e :: HTTP.HttpException) -> False) id eith

  when ((domain /= env.domain1) && (domain /= env.domain2)) $ do
    retryRequestUntil checkServiceIsUpReq "Federator ingress"

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

data ServiceInstance = ServiceInstance
  { handle :: ProcessHandle,
    config :: FilePath
  }

timeout :: Int -> IO a -> IO (Maybe a)
timeout usecs action = either (const Nothing) Just <$> race (threadDelay usecs) action

cleanupService :: (HasCallStack) => ServiceInstance -> IO ()
cleanupService inst = do
  mPid <- getPid inst.handle
  for_ mPid (signalProcess keyboardSignal)
  timeout 50000 (waitForProcess inst.handle) >>= \case
    Just _ -> pure ()
    Nothing -> do
      for_ mPid (signalProcess killProcess)
      void $ waitForProcess inst.handle
  whenM (doesFileExist inst.config) $ removeFile inst.config
  whenM (doesDirectoryExist inst.config) $ removeDirectoryRecursive inst.config

-- | Wait for a service to come up.
waitUntilServiceIsUp :: (HasCallStack) => Maybe ProcessDebug -> String -> Service -> App ()
waitUntilServiceIsUp mDebug domain srv =
  retryRequestUntilDebug
    mDebug
    (checkServiceIsUp domain srv)
    (show srv)

-- | Check if a service is up and running.
checkServiceIsUp :: String -> Service -> App Bool
checkServiceIsUp _ Nginz = pure True
checkServiceIsUp domain srv = do
  req <- baseRequest domain srv Unversioned "/i/status"
  checkStatus <- appToIO $ do
    res <- submit "GET" req
    pure (res.status `elem` [200, 204])
  eith <- liftIO (E.try checkStatus)
  pure $ either (\(_e :: HTTP.HttpException) -> False) id eith

withProcess :: (HasCallStack) => BackendResource -> ServiceOverrides -> Service -> Codensity App ()
withProcess resource overrides service = do
  let domain = berDomain resource
  sm <- lift $ getServiceMap domain
  env <- lift ask
  getConfig <-
    lift . appToIO $
      readServiceConfig service
        >>= updateServiceMapInConfig resource service
        >>= lookupConfigOverride overrides service
  let execName = configName service
  let (cwd, exe) = case env.servicesCwdBase of
        Nothing -> (Nothing, execName)
        Just dir ->
          (Just (dir </> execName), "../../dist" </> execName)

  startNginzLocalIO <- lift $ appToIO $ startNginzLocal resource

  stdOut <- liftIO $ newIORef []
  stdErr <- liftIO $ newIORef []
  phRef <- liftIO $ newIORef Nothing

  let prefix = "[" <> execName <> "@" <> domain <> maybe "" (":" <>) env.currentTestName <> "] "
  let initProcess = case (service, cwd) of
        (Nginz, Nothing) -> startNginzK8s domain sm
        (Nginz, Just _) -> startNginzLocalIO
        _ -> do
          config <- getConfig
          tempFile <- writeTempFile "/tmp" (execName <> "-" <> domain <> "-" <> ".yaml") (cs $ Yaml.encode config)
          (_, Just stdoutHdl, Just stderrHdl, ph) <- createProcess (proc exe ["-c", tempFile]) {cwd = cwd, std_out = CreatePipe, std_err = CreatePipe}
          let colorize = fromMaybe id (lookup execName processColors)
          void $ forkIO $ logToConsoleDebug (Just stdOut) colorize prefix stdoutHdl
          void $ forkIO $ logToConsoleDebug (Just stdErr) colorize prefix stderrHdl
          liftIO $ writeIORef phRef (Just ph)
          pure $ ServiceInstance ph tempFile

  void $
    hoistCodensity $
      Codensity $
        E.bracket initProcess cleanupService

  lift $
    addFailureContext ("Waiting for service: " <> prefix) $ do
      waitUntilServiceIsUp (Just $ ProcessDebug {phRef = phRef, stdOut = stdOut, stdErr = stdErr}) domain service

logToConsole :: (String -> String) -> String -> Handle -> IO ()
logToConsole = logToConsoleDebug Nothing

logToConsoleDebug :: Maybe (IORef [String]) -> (String -> String) -> String -> Handle -> IO ()
logToConsoleDebug mOutput colorize prefix hdl = do
  let go =
        do
          line <- hGetLine hdl
          putStrLn (colorize (prefix <> line))
          case mOutput of
            Nothing -> pure ()
            Just output -> do
              modifyIORef output (<> [line])
          go
          `E.catch` (\(_ :: E.IOException) -> pure ())
  go

retryRequestUntil :: (HasCallStack) => ((HasCallStack) => App Bool) -> String -> App ()
retryRequestUntil = retryRequestUntilDebug Nothing

data ProcessDebug = ProcessDebug
  { phRef :: IORef (Maybe ProcessHandle),
    stdOut :: IORef [String],
    stdErr :: IORef [String]
  }

retryRequestUntilDebug :: (HasCallStack) => Maybe ProcessDebug -> ((HasCallStack) => App Bool) -> String -> App ()
retryRequestUntilDebug mProcessDebug reqAction err = do
  isUp <-
    retrying
      (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
      (\_ isUp -> pure (not isUp))
      (const reqAction)
  unless isUp $ do
    case mProcessDebug of
      Nothing ->
        assertFailure ("Timed out waiting for service " <> err <> " to come up")
      Just (ProcessDebug {..}) -> do
        stdOut' <- liftIO $ readIORef stdOut
        stdErr' <- liftIO $ readIORef stdErr
        mPh <- liftIO $ readIORef phRef
        let stdOutStr = List.intercalate "\n" stdOut'
            stdErrStr = List.intercalate "\n" stdErr'
        mExitCode <- liftIO $ case mPh of
          Nothing -> pure Nothing
          Just ph -> Just <$> getProcessExitCode ph
        let msg =
              "Timed out waiting for service "
                <> err
                <> " to come up\n"
                <> "stdout:\n"
                <> stdOutStr
                <> "\nstderr:\n"
                <> stdErrStr
                <> "\nexitCode:\n"
                <> show (maybe "no exit code" show mExitCode)
        addFailureContext msg $
          assertFailure msg

startNginzK8s :: String -> ServiceMap -> IO ServiceInstance
startNginzK8s domain sm = do
  tmpDir <- liftIO $ createTempDirectory "/tmp" ("nginz" <> "-" <> domain)
  liftIO $
    copyDirectoryRecursively "/etc/wire/nginz/" tmpDir

  let nginxConfFile = tmpDir </> "conf" </> "nginx.conf"
  conf <- Text.readFile nginxConfFile
  let conf' =
        conf
          & Text.replace "access_log /dev/stdout" "access_log /dev/null"
          -- FUTUREWORK: Get these ports out of config
          & Text.replace ("listen 8080;\n    listen 8081 proxy_protocol;") (cs $ "listen " <> show sm.nginz.port <> ";")
          & Text.replace ("listen 8082;") (cs $ "listen unix:" <> (tmpDir </> "metrics-socket") <> ";")
          & Text.replace ("/var/run/nginz.pid") (cs $ tmpDir </> "nginz.pid")

  Text.writeFile nginxConfFile $ replaceUpstreamsInConfig conf' sm

  ph <- startNginz domain nginxConfFile "/"
  pure $ ServiceInstance ph tmpDir

startNginzLocal :: BackendResource -> App ServiceInstance
startNginzLocal resource = do
  let domain = berDomain resource
  sm <- getServiceMap domain

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

  -- hide access log
  let nginxConfFile = tmpDir </> "conf" </> "nginz" </> "nginx.conf"
  liftIO $ do
    conf <- Text.readFile nginxConfFile
    Text.writeFile nginxConfFile $
      ( conf
          & Text.replace "access_log /dev/stdout" "access_log /dev/null"
      )

  -- override port configuration
  let nginzPort = sm.nginz.port
      http2Port = berNginzHttp2Port resource
      sslPort = berNginzSslPort resource
      portConfig =
        [i|listen #{nginzPort};
            listen #{http2Port};
            listen #{sslPort} ssl;
            listen [::]:#{sslPort} ssl;
            http2 on;
        |]
      integrationConfFile = tmpDir </> "conf" </> "nginz" </> "integration.conf"

  liftIO $ do
    whenM (doesFileExist integrationConfFile) $ removeFile integrationConfFile
    writeFile integrationConfFile portConfig

  -- override upstreams
  let upstreamsCfg = tmpDir </> "conf" </> "nginz" </> "upstreams"

  liftIO $ do
    whenM (doesFileExist upstreamsCfg) $
      removeFile upstreamsCfg
    writeFile upstreamsCfg (makeUpstreamsCfgs sm)

  -- override pid configuration
  let pidConfigFile = tmpDir </> "conf" </> "nginz" </> "pid.conf"
  let pid = tmpDir </> "conf" </> "nginz" </> "nginz.pid"
  liftIO $ do
    whenM (doesFileExist $ pidConfigFile) $ removeFile pidConfigFile
    writeFile pidConfigFile (cs $ "pid " <> pid <> ";")

  -- start service
  ph <- liftIO $ startNginz domain nginxConfFile tmpDir

  -- return handle and nginx tmp dir path
  pure $ ServiceInstance ph tmpDir

makeUpstreamsCfgs :: ServiceMap -> String
makeUpstreamsCfgs sm =
  let upstreamTemplate _name _port =
        [i|upstream #{_name} {
            least_conn;
            keepalive 32;
            server 127.0.0.1:#{_port} max_fails=3 weight=1;
            }
        |]
   in concat
        $ (flip map)
          [ (serviceName Brig, sm.brig.port),
            (serviceName Cannon, sm.cannon.port),
            (serviceName Cargohold, sm.cargohold.port),
            (serviceName Galley, sm.galley.port),
            (serviceName Gundeck, sm.gundeck.port),
            (serviceName Nginz, sm.nginz.port),
            (serviceName WireProxy, sm.proxy.port),
            (serviceName Spar, sm.spar.port),
            ("federator_external", sm.federatorExternal.port)
          ]
        $ uncurry upstreamTemplate

-- | replace 'upstream <name> { ... }' blocks
replaceUpstreamsInConfig :: Text.Text -> ServiceMap -> Text.Text
replaceUpstreamsInConfig nginxConf sm =
  insertGeneratedUpstreams generateUpstreamsText $ removeUpstreamBlocks
  where
    -- remove blocks like:
    --
    -- upstream <name> {
    --   ...
    --   <config>
    --   ...
    -- }
    --
    -- Prerequisite is that the upstream  block itself does not contain block
    -- delimiters. AFAIK this usually holds.
    removeUpstreamBlocks :: Text.Text
    removeUpstreamBlocks =
      replaceAll "" $
        -- regex-tdfa does unfortunately not support shorthands for character classes.
        nginxConf *=~ [re|upstream[[:blank:]]+[[:word:]]+([[:blank:]]|[[:cntrl:]])+{([^}]|[[:cntrl:]])+}|]

    -- Insert generated upstreams:
    -- Try to put them right after the opening 'http {'.
    insertGeneratedUpstreams :: Text.Text -> Text.Text -> Text.Text
    insertGeneratedUpstreams ups conf =
      case Text.breakOn "http {" conf of
        (pre, rest)
          | not (Text.null rest) ->
              let (httpOpen, postOpen) = Text.splitAt (Text.length "http {") rest
               in Text.concat [pre, httpOpen, "\n\n", ups, postOpen]
        _ -> Text.concat [ups, "\n", conf]

    generateUpstreamsText :: Text.Text
    generateUpstreamsText =
      Text.concat $
        [ (serviceName Brig, sm.brig.port),
          (serviceName Cannon, sm.cannon.port),
          (serviceName Cargohold, sm.cargohold.port),
          (serviceName Galley, sm.galley.port),
          (serviceName Gundeck, sm.gundeck.port),
          (serviceName Nginz, sm.nginz.port),
          (serviceName WireProxy, sm.proxy.port),
          (serviceName Spar, sm.spar.port)
        ]
          <&> Text.pack . uncurry upstreamTemplate
      where
        upstreamTemplate _name _port =
          [i|upstream #{_name} {
              least_conn;
              keepalive 32;
              server 127.0.0.1:#{_port} max_fails=3 weight=1;
              }
          |]

startNginz :: String -> FilePath -> FilePath -> IO ProcessHandle
startNginz domain conf workingDir = do
  (_, Just stdoutHdl, Just stderrHdl, ph) <-
    createProcess
      (proc "nginx" ["-c", conf, "-g", "daemon off;", "-e", "/dev/stdout"])
        { cwd = Just workingDir,
          std_out = CreatePipe,
          std_err = CreatePipe
        }

  let prefix = "[" <> "nginz" <> "@" <> domain <> "] "
  let colorize = fromMaybe id (lookup "nginx" processColors)

  void $ forkIO $ logToConsole colorize prefix stdoutHdl
  void $ forkIO $ logToConsole colorize prefix stderrHdl

  pure ph
