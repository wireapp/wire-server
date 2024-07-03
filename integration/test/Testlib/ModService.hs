{-# LANGUAGE OverloadedStrings #-}

module Testlib.ModService
  ( withModifiedBackend,
    startDynamicBackend,
    startDynamicBackends,
    traverseConcurrentlyCodensity,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Codensity
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import Data.Default
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable
import qualified Data.Yaml as Yaml
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath
import System.IO
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Posix (keyboardSignal, killProcess, signalProcess)
import System.Process
import Testlib.App
import Testlib.HTTP
import Testlib.JSON
import Testlib.Printing
import Testlib.ResourcePool
import Testlib.Types
import Text.RawString.QQ
import Prelude

withModifiedBackend :: (HasCallStack) => ServiceOverrides -> ((HasCallStack) => String -> App a) -> App a
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
    Codensity $ \k -> do
      k' <- appToIOKleisli k
      liftIO $ withAsync (runAction x) k'

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

startDynamicBackends :: [ServiceOverrides] -> ([String] -> App a) -> App a
startDynamicBackends beOverrides k =
  runCodensity
    do
      when (Prelude.length beOverrides > 3) $ lift $ failApp "Too many backends. Currently only 3 are supported."
      pool <- asks (.resourcePool)
      resources <- acquireResources (Prelude.length beOverrides) pool
      void $ traverseConcurrentlyCodensity (uncurry startDynamicBackend) (zip resources beOverrides)
      pure $ map (.berDomain) resources
    k

startDynamicBackend :: BackendResource -> ServiceOverrides -> Codensity App ()
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
  startBackend resource overrides
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
    [(srv, berInternalServicePorts resource srv :: Int) | srv <- allServices]

startBackend ::
  (HasCallStack) =>
  BackendResource ->
  ServiceOverrides ->
  Codensity App ()
startBackend resource overrides = do
  traverseConcurrentlyCodensity (withProcess resource overrides) allServices
  lift $ ensureBackendReachable resource.berDomain

ensureBackendReachable :: String -> App ()
ensureBackendReachable domain = do
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

cleanupService :: ServiceInstance -> IO ()
cleanupService inst = do
  let ignoreExceptions action = E.catch action $ \(_ :: E.SomeException) -> pure ()
  ignoreExceptions $ do
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
waitUntilServiceIsUp :: String -> Service -> App ()
waitUntilServiceIsUp domain srv =
  retryRequestUntil
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

withProcess :: BackendResource -> ServiceOverrides -> Service -> Codensity App ()
withProcess resource overrides service = do
  let domain = berDomain resource
  sm <- lift $ getServiceMap domain
  getConfig <-
    lift . appToIO $
      readServiceConfig service
        >>= updateServiceMapInConfig resource service
        >>= lookupConfigOverride overrides service
  let execName = configName service
  (cwd, exe) <-
    lift $ asks \env -> case env.servicesCwdBase of
      Nothing -> (Nothing, execName)
      Just dir ->
        (Just (dir </> execName), "../../dist" </> execName)

  startNginzLocalIO <- lift $ appToIO $ startNginzLocal resource

  let initProcess = case (service, cwd) of
        (Nginz, Nothing) -> startNginzK8s domain sm
        (Nginz, Just _) -> startNginzLocalIO
        _ -> do
          config <- getConfig
          tempFile <- writeTempFile "/tmp" (execName <> "-" <> domain <> "-" <> ".yaml") (cs $ Yaml.encode config)
          (_, Just stdoutHdl, Just stderrHdl, ph) <-
            createProcess
              ( proc
                  exe
                  [ "-c",
                    tempFile,
                    "+RTS",
                    "-pj",
                    "-hy",
                    "-l",
                    "--eventlog-flush-interval=3"
                  ]
              )
                { cwd = cwd,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          let prefix = "[" <> execName <> "@" <> domain <> "] "
          let colorize = fromMaybe id (lookup execName processColors)
          void $ forkIO $ logToConsole colorize prefix stdoutHdl
          void $ forkIO $ logToConsole colorize prefix stderrHdl
          pure $ ServiceInstance ph tempFile

  void $ Codensity $ \k -> do
    iok <- appToIOKleisli k
    liftIO $ E.bracket initProcess cleanupService iok

  lift $ waitUntilServiceIsUp domain service

logToConsole :: (String -> String) -> String -> Handle -> IO ()
logToConsole colorize prefix hdl = do
  let go =
        do
          line <- hGetLine hdl
          putStrLn (colorize (prefix <> line))
          go
          `E.catch` (\(_ :: E.IOException) -> pure ())
  go

retryRequestUntil :: (HasCallStack) => App Bool -> String -> App ()
retryRequestUntil reqAction err = do
  isUp <-
    retrying
      (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
      (\_ isUp -> pure (not isUp))
      (const reqAction)
  unless isUp $
    failApp ("Timed out waiting for service " <> err <> " to come up")

startNginzK8s :: String -> ServiceMap -> IO ServiceInstance
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
  pure $ ServiceInstance ph tmpDir

startNginzLocal :: BackendResource -> App ServiceInstance
startNginzLocal resource = do
  let domain = berDomain resource
      http2Port = berNginzHttp2Port resource
      sslPort = berNginzSslPort resource
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
  liftIO $ createUpstreamsCfg upstreamsCfg sm
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
  ph <- liftIO $ startNginz domain nginxConfFile tmpDir

  -- return handle and nginx tmp dir path
  pure $ ServiceInstance ph tmpDir

createUpstreamsCfg :: String -> ServiceMap -> IO ()
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
