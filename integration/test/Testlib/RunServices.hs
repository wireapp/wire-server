{-# OPTIONS_GHC -Wno-unused-matches #-}

module Testlib.RunServices where

import Control.Monad.Codensity (lowerCodensity)
import qualified Data.Map as Map
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.FilePath
import System.Posix (getWorkingDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Testlib.Prelude
import Testlib.ResourcePool
import Testlib.Run (createGlobalEnv)

backendA :: BackendResource
backendA =
  BackendResource
    { berBrigKeyspace = "brig_test",
      berGalleyKeyspace = "galley_test",
      berSparKeyspace = "spar_test",
      berGundeckKeyspace = "gundeck_test",
      berElasticsearchIndex = "directory_test",
      berFederatorInternal = 8097,
      berFederatorExternal = 8098,
      berDomain = "example.com",
      berAwsUserJournalQueue = "integration-user-events.fifo",
      berAwsPrekeyTable = "integration-brig-prekeys",
      berAwsS3Bucket = "dummy-bucket",
      berAwsQueueName = "integration-gundeck-events",
      berBrigInternalEvents = "integration-brig-events-internal",
      berEmailSMSSesQueue = "integration-brig-events",
      berEmailSMSEmailSender = "backend-integration@wire.com",
      berGalleyJournal = "integration-team-events.fifo",
      berVHost = "/",
      berNginzSslPort = 8443
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
      (Nginz, 8080)
    ]

backendB :: BackendResource
backendB =
  BackendResource
    { berBrigKeyspace = "brig_test2",
      berGalleyKeyspace = "galley_test2",
      berSparKeyspace = "spar_test2",
      berGundeckKeyspace = "gundeck_test2",
      berElasticsearchIndex = "directory2_test",
      berFederatorInternal = 9097,
      berFederatorExternal = 9098,
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
      berVHost = "/",
      berNginzSslPort = 9443
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
      (Nginz, 9080)
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
    Nothing -> error "Could not find project root. Please make sure you call run-services from the somewhere in wire-server."
    Just projectRoot ->
      pure $ joinPath [projectRoot, "services/integration.yaml"]

  genv <- createGlobalEnv cfg
  env <- lowerCodensity $ mkEnv genv

  args <- getArgs
  let args' = case args of
        (x : xs) -> x : xs
        _ -> ["sleep", "10000d"]
  let cp = proc "sh" (["-c", "exec \"$@\"", "--"] <> args')

  runAppWithEnv env $ do
    lowerCodensity $ do
      _modifyEnv <- traverseConcurrentlyCodensity (\(res, staticPorts) -> startDynamicBackend res staticPorts def) [(backendA, staticPortsA), (backendB, staticPortsB)]
      liftIO $ do
        (_, _, _, ph) <- createProcess cp
        exitWith =<< waitForProcess ph
