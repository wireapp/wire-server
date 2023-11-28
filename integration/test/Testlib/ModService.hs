{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Testlib.ModService
  ( withModifiedBackend,
    startDynamicBackend,
    startDynamicBackends,
    traverseConcurrentlyCodensity,
  )
where

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
import Data.Aeson.KeyMap qualified as Aeson
import Data.Default
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
import System.Exit (ExitCode)
import System.FilePath
import System.IO
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Posix (killProcess, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), cleanupProcess, createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)
import Testlib.App
import Testlib.HTTP
import Testlib.JSON
import Testlib.Printing
import Testlib.ResourcePool
import Testlib.Types
import Text.RawString.QQ
import UnliftIO.Exception qualified as UnliftIO
import Prelude

withModifiedBackend :: HasCallStack => ServiceOverrides -> (HasCallStack => String -> App a) -> App a
withModifiedBackend overrides k =
  startDynamicBackends [overrides] (\domains -> k (head domains))

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
  let overrides =
        mconcat
          [ setKeyspace,
            setEsIndex,
            setFederationSettings,
            setAwsConfigs,
            setLogLevel,
            beOverrides
          ]
  startBackend resource overrides allServices
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
          gundeckCfg = setField "settings.federationDomain" resource.berDomain,
          backgroundWorkerCfg =
            setField "federatorInternal.port" resource.berFederatorInternal
              >=> setField "federatorInternal.host" ("127.0.0.1" :: String)
              >=> setField "rabbitmq.vHost" resource.berVHost,
          federatorInternalCfg =
            setField "federatorInternal.port" resource.berFederatorInternal
              >=> setField "federatorExternal.port" resource.berFederatorExternal
              >=> setField "optSettings.setFederationDomain" resource.berDomain
        }

    setKeyspace :: ServiceOverrides
    setKeyspace =
      def
        { galleyCfg = setField "cassandra.keyspace" resource.berGalleyKeyspace,
          brigCfg = setField "cassandra.keyspace" resource.berBrigKeyspace,
          sparCfg = setField "cassandra.keyspace" resource.berSparKeyspace,
          gundeckCfg = setField "cassandra.keyspace" resource.berGundeckKeyspace
        }

    setEsIndex :: ServiceOverrides
    setEsIndex =
      def
        { brigCfg = setField "elasticsearch.index" resource.berElasticsearchIndex
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
          federatorInternalCfg = setField "logLevel" ("Warn" :: String)
        }

startBackend ::
  HasCallStack =>
  BackendResource ->
  ServiceOverrides ->
  [Service] ->
  Codensity App (Env -> Env)
startBackend resource overrides services = do
  let domain = resource.berDomain

      updateServiceMapInConfig :: Service -> Value -> App Value
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
          [(srv, berInternalServicePorts resource srv :: Int) | srv <- services]

      serviceMap =
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
                -- FUTUREWORK: Set to g Proxy, when we add Proxy to spawned services
                proxy = HostPort "127.0.0.1" 9087,
                stern = g Stern
              }

      initService :: MVar () -> Service -> App RunningService
      initService lock original = do
        -- start up the service
        running <- case original of
          Nginz ->
            asks servicesCwdBase >>= \case
              Nothing -> startNginzK8s domain serviceMap
              Just _ -> startNginzLocal domain resource.berNginzHttp2Port resource.berNginzSslPort serviceMap
          srv ->
            readServiceConfig srv
              >>= updateServiceMapInConfig srv
              >>= lookupConfigOverride overrides srv
              >>= startProcess domain srv

        -- wait for the service to come up
        let checkServiceIsUp = \case
              Nginz -> pure True
              srv -> do
                req <- baseRequest domain srv Unversioned "/i/status"
                checkStatus <- appToIO $ do
                  res <- submit "GET" req
                  pure (res.status `elem` [200, 204])
                eith <- liftIO (E.try checkStatus)
                pure $ either (\(_e :: HTTP.HttpException) -> False) id eith

        waitForService <-
          appToIOKleisli
            \srv ->
              retryRequestUntil
                do checkServiceIsUp srv
                do show srv

        liftIO do waitForService original
        ensureReachable resource.berDomain

        -- the service has come up, we can safely put the lock in
        liftIO do putMVar lock ()

        -- return the running service
        pure running

      -- run a service to completion
      waitService :: RunningService -> App ExitCode
      waitService = liftIO . waitForProcess . processHandle

      -- terminate a process
      closeService :: RunningService -> IO ()
      closeService runningProcess = do
        -- acquire handles for the running process
        -- (stdout, stderr, process handle)
        let hdls = extractHandles runningProcess
            ph = processHandle runningProcess
            path = configPath runningProcess
        -- try killing the process while cleaning up the handlers
        cleanupProcess hdls
        timeout 50000 (waitForProcess ph) >>= \case
          Just _ -> pure ()
          Nothing -> do
            putStrLn "Process has not terminated before timing out the first time"
            timeout 100000 (waitForProcess ph) >>= \case
              Just _ -> pure ()
              Nothing -> do
                putStrLn "Process has not terminated before timing out the second time"
                mPid <- getPid ph
                for_ mPid (signalProcess killProcess)
                void $ waitForProcess ph
        whenM (doesFileExist path) $ removeFile path
        whenM (doesDirectoryExist path) $ removeDirectoryRecursive path

      -- bracketing services with a lock
      -- the three arguments work as follows
      -- - acquire a lock by starting up a service and waiting for it to come up
      -- - kill the process (upon completion or error)
      -- - wait for either
      --   - the lock to be released (asking for the service to be terminated)
      --   - the process to run to completion
      bracketServiceWithLock lock ser =
        UnliftIO.bracket
          do initService lock ser
          do liftIO . closeService
          \running -> do
            waitServiceIO <- appToIO do
              waitService running
            liftIO do
              -- racing termination from the outside
              -- and the actual service
              race
                -- wait for the lock to be freed again (meaning release the process)
                do putMVar lock ()
                -- run the process to finish
                do waitServiceIO
              >>= liftIO . either
                do const (putStrLn "process has been terminated")
                \code -> putStrLn do
                  "process finished with exit code " <> show code

      modifyEnv env = env {serviceMap = Map.insert domain serviceMap env.serviceMap}

  lift $ local modifyEnv do
    bracketServiceWithLockIO <- appToIOKleisli (uncurry bracketServiceWithLock)

    liftIO do
      -- acquire empty locks that will later be filled by starting up the services
      locks :: [(MVar (), Service)] <- for services \service -> do
        lock <- liftIO newEmptyMVar
        pure (lock, service)

      -- for each lock, fill the lock and continue
      mapConcurrently_ bracketServiceWithLockIO locks
        -- if running the services throws an exception, we empty all locks
        -- and allow the race with the actual services to succeed and hence
        -- terminate the service
        `finally` traverse (takeMVar . fst) locks

  pure modifyEnv
  where
    ensureReachable :: String -> App ()
    ensureReachable domain = do
      env <- ask
      let checkServiceIsUpReq = do
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
              -- If we get 533 here it means federation is not avaiable between domains
              -- but ingress is working, since we're processing the request.
              let is200 = res.status == 200
                  msg = case res.jsonBody of
                    Just (Object obj) ->
                      (Aeson.lookup "message" obj)
                    _ -> Nothing
                  isFedDenied =
                    res.status == 533
                      && ( Text.isInfixOf
                             "federation-denied"
                             (Text.pack $ show msg)
                         )

              pure (is200 || isFedDenied)
            eith <- liftIO (E.try checkStatus)
            pure $ either (\(_e :: HTTP.HttpException) -> False) id eith

      when ((domain /= env.domain1) && (domain /= env.domain2)) $ do
        retryRequestUntil checkServiceIsUpReq "Federator ingress"

startProcess :: String -> Service -> Value -> App RunningService
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

data RunningService = MkRunningService
  { -- | the process handle of the process
    processHandle :: ProcessHandle,
    -- | the path in /tmp where the config file of the service lives
    configPath :: FilePath,
    -- | the stdout handler usually created by 'startProcess''
    stdoutHdl :: Handle,
    -- | the stderr handler usually created by 'startProcess''
    stderrHdl :: Handle
  }

extractHandles :: RunningService -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
extractHandles MkRunningService {processHandle, stdoutHdl, stderrHdl} =
  (Nothing, Just stdoutHdl, Just stderrHdl, processHandle)

startProcess' :: String -> String -> Value -> App RunningService
startProcess' domain execName config = do
  tempFile <- liftIO $ writeTempFile "/tmp" (execName <> "-" <> domain <> "-" <> ".yaml") (cs $ Yaml.encode config)

  (cwd, exe) <-
    asks \env -> case env.servicesCwdBase of
      Nothing -> (Nothing, execName)
      Just dir ->
        (Just (dir </> execName), "../../dist" </> execName)

  (_, Just stdoutHdl, Just stderrHdl, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd, std_out = CreatePipe, std_err = CreatePipe}
  let prefix = "[" <> execName <> "@" <> domain <> "] "
  let colorize = fromMaybe id (lookup execName processColors)
  void $ liftIO $ forkIO $ logToConsole colorize prefix stdoutHdl
  void $ liftIO $ forkIO $ logToConsole colorize prefix stderrHdl
  pure (MkRunningService ph tempFile stdoutHdl stderrHdl)

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

retryRequestUntil :: HasCallStack => App Bool -> String -> App ()
retryRequestUntil reqAction err = do
  isUp <-
    retrying
      (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
      (\_ isUp -> pure (not isUp))
      (const reqAction)
  unless isUp $
    failApp ("Timed out waiting for service " <> err <> " to come up")

startNginzK8s :: String -> ServiceMap -> App RunningService
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
  (ph, stdoutHdl, stderrHdl) <- startNginz domain nginxConfFile "/"
  pure do
    MkRunningService ph tmpDir stdoutHdl stderrHdl

startNginzLocal :: String -> Word16 -> Word16 -> ServiceMap -> App RunningService
startNginzLocal domain http2Port sslPort sm = do
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

  -- override port configuration
  let portConfigTemplate =
        [r|listen {localPort};
listen {http2_port} http2;
listen {ssl_port} ssl http2;
listen [::]:{ssl_port} ssl http2;
|]
  let portConfig =
        portConfigTemplate
          & Text.replace "{localPort}" (cs $ show (sm.nginz.port))
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
  (ph, stdoutHdl, stderrHdl) <- startNginz domain nginxConfFile tmpDir

  -- return handle and nginx tmp dir path
  pure do
    MkRunningService ph tmpDir stdoutHdl stderrHdl

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
    \case
      (srv, p) -> do
        let upstream =
              upstreamTemplate
                & Text.replace "{name}" (cs $ srv)
                & Text.replace "{port}" (cs $ show p)
        liftIO $ appendFile upstreamsCfg (cs upstream)

startNginz :: String -> FilePath -> FilePath -> App (ProcessHandle, Handle, Handle)
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
  pure (ph, stdoutHdl, stderrHdl)
