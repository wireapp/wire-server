{-# OPTIONS_GHC -Wno-unused-matches #-}

module Testlib.RunServices where

import Control.Concurrent
import Control.Monad.Codensity (lowerCodensity)
import Data.Map qualified as Map
import SetupHelpers
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.FilePath
import System.Posix (getWorkingDirectory)
import System.Process
import Testlib.Ports qualified as Ports
import Testlib.Prelude
import Testlib.ResourcePool
import Testlib.Run (createGlobalEnv)

backendA :: BackendResource
backendA =
  BackendResource
    { berName = BackendA,
      berBrigKeyspace = "brig_test",
      berGalleyKeyspace = "galley_test",
      berSparKeyspace = "spar_test",
      berGundeckKeyspace = "gundeck_test",
      berElasticsearchIndex = "directory_test",
      berFederatorInternal = Ports.port (Ports.ServiceInternal FederatorInternal) BackendA,
      berFederatorExternal = Ports.port Ports.FederatorExternal BackendA,
      berDomain = "example.com",
      berAwsUserJournalQueue = "integration-user-events.fifo",
      berAwsPrekeyTable = "integration-brig-prekeys",
      berAwsS3Bucket = "dummy-bucket",
      berAwsQueueName = "integration-gundeck-events",
      berBrigInternalEvents = "integration-brig-events-internal",
      berEmailSMSSesQueue = "integration-brig-events",
      berEmailSMSEmailSender = "backend-integration@wire.com",
      berGalleyJournal = "integration-team-events.fifo",
      berVHost = "backendA",
      berNginzSslPort = Ports.port Ports.NginzSSL BackendA,
      berInternalServicePorts = Ports.internalServicePorts BackendA
    }

staticPortsA :: Map.Map Service Word16
staticPortsA =
  Map.fromList
    [ (Brig, 8082),
      (Galley, 8085),
      (Gundeck, 8086),
      (Cannon, 8083),
      (Cargohold, 8084),
      (Spar, 8088),
      (BackgroundWorker, 8089),
      (Nginz, 8080),
      (Stern, 8091)
    ]

backendB :: BackendResource
backendB =
  BackendResource
    { berName = BackendB,
      berBrigKeyspace = "brig_test2",
      berGalleyKeyspace = "galley_test2",
      berSparKeyspace = "spar_test2",
      berGundeckKeyspace = "gundeck_test2",
      berElasticsearchIndex = "directory2_test",
      berFederatorInternal = Ports.port (Ports.ServiceInternal FederatorInternal) BackendB,
      berFederatorExternal = Ports.port Ports.FederatorExternal BackendB,
      berDomain = "b.example.com",
      berAwsUserJournalQueue = "integration-user-events.fifo2",
      berAwsPrekeyTable = "integration-brig-prekeys2",
      berAwsS3Bucket = "dummy-bucket2",
      berAwsQueueName = "integration-gundeck-events2",
      berBrigInternalEvents = "integration-brig-events-internal2",
      berEmailSMSSesQueue = "integration-brig-events2",
      berEmailSMSEmailSender = "backend-integration2@wire.com",
      berGalleyJournal = "integration-team-events.fifo2",
      -- FUTUREWORK: set up vhosts in dev/ci for example.com and b.example.com
      -- in case we want backendA and backendB to federate with a third backend
      -- (because otherwise both queues will overlap)
      berVHost = "backendB",
      berNginzSslPort = Ports.port Ports.NginzSSL BackendB,
      berInternalServicePorts = Ports.internalServicePorts BackendB
    }

staticPortsB :: Map.Map Service Word16
staticPortsB =
  Map.fromList
    [ (Brig, 9082),
      (Galley, 9085),
      (Gundeck, 9086),
      (Cannon, 9083),
      (Cargohold, 9084),
      (Spar, 9088),
      (BackgroundWorker, 9089),
      (Nginz, 9080),
      (Stern, 9091)
    ]

parentDir :: FilePath -> Maybe FilePath
parentDir path =
  let dirs = splitPath path
   in if null dirs
        then Nothing
        else Just $ joinPath (init dirs)

containsGit :: FilePath -> IO Bool
containsGit path =
  doesDirectoryExist $ joinPath [path, ".git"]

findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot path = do
  c <- containsGit path
  if c
    then pure (Just path)
    else case parentDir path of
      Nothing -> pure Nothing
      Just p -> findProjectRoot p

main :: IO ()
main = do
  cwd <- getWorkingDirectory
  mbProjectRoot <- findProjectRoot cwd
  cfg <- case mbProjectRoot of
    Nothing -> error "Could not find project root. Please make sure you call run-services from somewhere in wire-server."
    Just projectRoot ->
      pure $ joinPath [projectRoot, "services/integration.yaml"]

  genv <- createGlobalEnv cfg
  env <- lowerCodensity $ mkEnv genv

  args <- getArgs

  let run = case args of
        [] -> do
          putStrLn "services started"
          forever (threadDelay 1000000000)
        _ -> do
          let cp = proc "sh" (["-c", "exec \"$@\"", "--"] <> args)
          (_, _, _, ph) <- createProcess cp
          exitWith =<< waitForProcess ph

  runAppWithEnv env $ do
    lowerCodensity $ do
      _modifyEnv <-
        traverseConcurrentlyCodensity
          ( \(res, staticPorts) ->
              -- We add the 'fullSerachWithAll' overrrides is a hack to get
              -- around https://wearezeta.atlassian.net/browse/WPB-3796
              startDynamicBackend res staticPorts fullSearchWithAll
          )
          [(backendA, staticPortsA), (backendB, staticPortsB)]
      liftIO run
