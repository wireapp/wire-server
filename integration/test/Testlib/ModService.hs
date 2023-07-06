{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
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
import qualified Control.Exception as E
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
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.String.Conversions (cs)
import Data.Text hiding (elem, head, map, zip)
import qualified Data.Text.IO as Text
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
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Testlib.App
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.Printing
import Testlib.ResourcePool
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
  (a -> Codensity App (Env -> Env)) ->
  ([a] -> Codensity App (Env -> Env))
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

startDynamicBackends :: [ServiceOverrides] -> ([String] -> App a) -> App a
startDynamicBackends beOverrides = runCodensity $ do
  when (Prelude.length beOverrides > 3) $ lift $ failApp "Too many backends. Currently only 3 are supported."
  pool <- asks (.resourcePool)
  resources <- acquireResources (Prelude.length beOverrides) pool
  void $ traverseConcurrentlyCodensity (\(res, overrides) -> startDynamicBackend res mempty overrides) (zip resources beOverrides)
  pure $ map (.berDomain) resources

startDynamicBackend :: BackendResource -> Map.Map Service Word16 -> ServiceOverrides -> Codensity App (Env -> Env)
startDynamicBackend resource staticPorts beOverrides = do
  defDomain <- asks (.domain1)
  let services =
        withOverrides beOverrides $
          Map.mapWithKey
            ( \srv conf ->
                conf
                  >=> setKeyspace srv
                  >=> setEsIndex srv
                  >=> setFederationSettings srv
                  >=> setAwsAdnQueuesConfigs srv
                  >=> setField "logLevel" "Warn"
            )
            defaultServiceOverridesToMap
  startBackend
    resource.berDomain
    staticPorts
    (Just resource.berNginzSslPort)
    (Just setFederatorConfig)
    services
    ( \ports sm -> do
        let templateBackend = fromMaybe (error "no default domain found in backends") $ sm & Map.lookup defDomain
         in Map.insert resource.berDomain (setFederatorPorts resource $ updateServiceMap ports templateBackend) sm
    )
  where
    setAwsAdnQueuesConfigs :: Service -> Value -> App Value
    setAwsAdnQueuesConfigs = \case
      Brig ->
        setField "aws.userJournalQueue" resource.berAwsUserJournalQueue
          >=> setField "aws.prekeyTable" resource.berAwsPrekeyTable
          >=> setField "internalEvents.queueName" resource.berBrigInternalEvents
          >=> setField "emailSMS.email.sesQueue" resource.berEmailSMSSesQueue
          >=> setField "emailSMS.general.emailSender" resource.berEmailSMSEmailSender
          >=> setField "rabbitmq.vHost" resource.berVHost
      Cargohold -> setField "aws.s3Bucket" resource.berAwsS3Bucket
      Gundeck -> setField "aws.queueName" resource.berAwsQueueName
      Galley ->
        setField "journal.queueName" resource.berGalleyJournal
          >=> setField "rabbitmq.vHost" resource.berVHost
      _ -> pure

    setFederationSettings :: Service -> Value -> App Value
    setFederationSettings =
      \case
        Brig ->
          setField "optSettings.setFederationDomain" resource.berDomain
            >=> setField
              "optSettings.setFederationDomainConfigs"
              [object ["domain" .= resource.berDomain, "search_policy" .= "full_search"]]
            >=> setField "federatorInternal.port" resource.berFederatorInternal
        Cargohold ->
          setField "settings.federationDomain" resource.berDomain
            >=> setField "federator.port" resource.berFederatorInternal
        Galley ->
          setField "settings.federationDomain" resource.berDomain
            >=> setField "settings.featureFlags.classifiedDomains.config.domains" [resource.berDomain]
            >=> setField "federator.port" resource.berFederatorInternal
        Gundeck -> setField "settings.federationDomain" resource.berDomain
        BackgroundWorker ->
          setField "federatorInternal.port" resource.berFederatorInternal
            >=> setField "rabbitmq.vHost" resource.berVHost
        _ -> pure

    setFederatorConfig :: Value -> App Value
    setFederatorConfig =
      setField "federatorInternal.port" resource.berFederatorInternal
        >=> setField "federatorExternal.port" resource.berFederatorExternal
        >=> setField "optSettings.setFederationDomain" resource.berDomain

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

setFederatorPorts :: BackendResource -> ServiceMap -> ServiceMap
setFederatorPorts resource sm =
  sm
    { federatorInternal = sm.federatorInternal {host = "127.0.0.1", port = resource.berFederatorInternal},
      federatorExternal = sm.federatorExternal {host = "127.0.0.1", port = resource.berFederatorExternal}
    }

withModifiedServices :: Map.Map Service (Value -> App Value) -> Codensity App String
withModifiedServices services = do
  domain <- lift $ asks (.domain1)
  void $
    startBackend domain mempty Nothing Nothing services (\ports -> Map.adjust (updateServiceMap ports) domain)
  pure domain

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
          BackgroundWorker -> sm {backgroundWorker = sm.backgroundWorker {host = "127.0.0.1", port = newPort}}
    )
    serviceMap
    ports

startBackend ::
  HasCallStack =>
  String ->
  Map.Map Service Word16 ->
  Maybe Word16 ->
  Maybe (Value -> App Value) ->
  Map.Map Service (Value -> App Value) ->
  (Map.Map Service Word16 -> Map.Map String ServiceMap -> Map.Map String ServiceMap) ->
  Codensity App (Env -> Env)
startBackend domain staticPorts nginzSslPort mFederatorOverrides services modifyBackends = do
  ports <-
    Map.traverseWithKey
      ( \srv _ ->
          case Map.lookup srv staticPorts of
            Just port -> pure (port, Nothing)
            Nothing -> do
              (port, sock) <- liftIO openFreePort
              pure (fromIntegral port, Just sock)
      )
      services

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
              case (srv, forSrv) of
                (Spar, Just Spar) -> do
                  overridden
                    -- FUTUREWORK: override "saml.spAppUri" and "saml.spSsoUri" with correct port, too?
                    & setField "saml.spHost" ("127.0.0.1" :: String)
                    & setField "saml.spPort" port
                _ -> pure overridden
          )
          config
          (Map.assocs ports)

  -- close all sockets before starting the services
  stopInstances <- lift $ do
    for_ (Map.keys services) $ \srv ->
      case Map.lookup srv ports of
        Just (_, mbSocket) ->
          for_ mbSocket $ \socket ->
            liftIO $ N.close socket
        Nothing -> failApp "the impossible in withServices happened"

    fedInstance <-
      case mFederatorOverrides of
        Nothing -> pure []
        Just override ->
          readServiceConfig' "federator"
            >>= updateServiceMapInConfig Nothing
            >>= override
            >>= startProcess' domain "federator"
            <&> (: [])

    otherInstances <- for (Map.assocs services) $ \case
      (Nginz, _) -> do
        env <- ask
        sm <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup domain (modifyBackends (fromIntegral . fst <$> ports) env.serviceMap))
        port <- maybe (failApp "the impossible in withServices happened") (pure . fromIntegral . fst) (Map.lookup Nginz ports)
        startNginz domain port nginzSslPort sm
      (srv, modifyConfig) -> do
        readServiceConfig srv
          >>= updateServiceMapInConfig (Just srv)
          >>= modifyConfig
          >>= startProcess domain srv

    let instances = fedInstance <> otherInstances

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

  let modifyEnv env =
        env {serviceMap = modifyBackends (fromIntegral . fst <$> ports) env.serviceMap}

  Codensity $ \action -> local modifyEnv $ do
    waitForService <- appToIOKleisli (waitUntilServiceUp domain)
    ioAction <- appToIO (action ())
    liftIO $
      (mapConcurrently_ waitForService (Map.keys ports) >> ioAction)
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

  tempFile <- liftIO $ writeTempFile "/tmp" (execName <> "-" <> domain <> "-" <> ".yaml") (cs $ Yaml.encode config)

  (cwd, exe) <-
    asks (.servicesCwdBase) <&> \case
      Nothing -> (Nothing, execName)
      Just dir ->
        (Just (dir </> execName), "../../dist" </> execName)

  (_, Just stdoutHdl, Just stderrHdl, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd, env = Just processEnv, std_out = CreatePipe, std_err = CreatePipe}
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
startNginz domain localPort mSslPort sm = do
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
          & replace (cs "access_log /dev/stdout") (cs "access_log /dev/null")
      )

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

  http2Port :: Word16 <- liftIO $ (openFreePort >>= \(p, s) -> N.close s $> fromIntegral p)
  -- We've seen nginx erroring that it cannot bind() to http2Port.
  -- When this delay is added the problem doesn't occur anymore locally .
  liftIO $ threadDelay 100000

  -- override port configuration
  let portConfigTemplate =
        cs $
          [r|listen {localPort};
listen {http2_port} http2;
listen {ssl_port} ssl http2;
listen [::]:{ssl_port} ssl http2;
|]
  let portConfig =
        portConfigTemplate
          & replace (cs "{localPort}") (cs $ show localPort)
          & replace (cs "{http2_port}") (cs $ show http2Port)
          & replace (cs "{ssl_port}") (cs $ show sslPort)

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
            & replace (cs "{port}") (cs $ show sm.federatorExternal.port)
      )

  -- override pid configuration
  let pidConfigFile = tmpDir </> "conf" </> "nginz" </> "pid.conf"
  let pid = tmpDir </> "conf" </> "nginz" </> "nginz.pid"
  liftIO $ do
    whenM (doesFileExist $ pidConfigFile) $ removeFile pidConfigFile
    writeFile pidConfigFile (cs $ "pid " <> pid <> ";")

  -- start service

  (_, Just stdoutHdl, Just stderrHdl, ph) <-
    liftIO $
      createProcess
        (proc "nginx" ["-c", tmpDir </> "conf" </> "nginz" </> "nginx.conf", "-p", tmpDir, "-g", "daemon off;", "-e", "/dev/stdout"])
          { cwd = Just $ cs tmpDir,
            std_out = CreatePipe,
            std_err = CreatePipe
          }

  let prefix = "[" <> "nginz" <> "@" <> domain <> "] "
  let colorize = fromMaybe id (lookup "nginx" processColors)
  void $ liftIO $ forkIO $ logToConsole colorize prefix stdoutHdl
  void $ liftIO $ forkIO $ logToConsole colorize prefix stderrHdl

  -- return handle and nginx tmp dir path
  pure (ph, tmpDir)
