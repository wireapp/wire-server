module Testlib.ResourcePool
  ( ResourcePool,
    BackendResource (..),
    backendResources,
    remoteDomains,
    createBackendResourcePool,
    acquireResources,
    releaseResources,
  )
where

import Control.Concurrent
import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.List.Extra ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Tuple
import Data.Word
import System.IO
import Prelude

type MLock = MVar ()

data ResourcePool a = ResourcePool
  { lock :: MLock,
    sem :: QSemN,
    resources :: IORef (Set.Set a)
  }

newResourcePool :: Ord a => Set a -> IO (ResourcePool a)
newResourcePool xs =
  ResourcePool
    <$> newMVar ()
    <*> newQSemN (length xs)
    <*> newIORef xs

acquireResources :: Ord a => Int -> ResourcePool a -> IO (Set a)
acquireResources n pool = withMVar pool.lock $ \_ -> do
  waitQSemN pool.sem n
  atomicModifyIORef pool.resources $ swap . Set.splitAt n

releaseResources :: Ord a => ResourcePool a -> [a] -> IO ()
releaseResources pool xs = withMVar pool.lock $ \_ -> do
  modifyIORef pool.resources $ Set.union (Set.fromList xs)
  signalQSemN pool.sem (length xs)

createBackendResourcePool :: IO (ResourcePool BackendResource)
createBackendResourcePool = newResourcePool (backendResources 3)

data BackendResource = BackendResource
  { berBrigKeyspace :: String,
    berGalleyKeyspace :: String,
    berSparKeyspace :: String,
    berGundeckKeyspace :: String,
    berElasticsearchIndex :: String,
    berFederatorInternal :: Word16,
    berFederatorExternal :: Word16,
    berDomain :: String,
    berAwsUserJournalQueue :: String,
    berAwsPrekeyTable :: String,
    berAwsS3Bucket :: String,
    berAwsQueueName :: String,
    berBrigInternalEvents :: String,
    berEmailSMSSesQueue :: String,
    berEmailSMSEmailSender :: String,
    berGalleyJournal :: String,
    berVHost :: String,
    berNginzSslPort :: Word16
  }
  deriving (Show, Eq, Ord)

backendResources :: Word16 -> Set.Set BackendResource
backendResources n =
  [1 .. n]
    <&> ( \i ->
            BackendResource
              { berBrigKeyspace = "brig_test_dyn_" <> show i,
                berGalleyKeyspace = "galley_test_dyn_" <> show i,
                berSparKeyspace = "spar_test_dyn_" <> show i,
                berGundeckKeyspace = "gundeck_test_dyn_" <> show i,
                berElasticsearchIndex = "directory_dyn_" <> show i <> "_test",
                berFederatorInternal = federatorInternalPort i,
                berFederatorExternal = federatorExternalPort i,
                berDomain = domain i,
                berAwsUserJournalQueue = "integration-user-events.fifo" <> suffix i,
                berAwsPrekeyTable = "integration-brig-prekeys" <> suffix i,
                berAwsS3Bucket = "dummy-bucket" <> suffix i,
                berAwsQueueName = "integration-gundeck-events" <> suffix i,
                berBrigInternalEvents = "integration-brig-events-internal" <> suffix i,
                berEmailSMSSesQueue = "integration-brig-events" <> suffix i,
                berEmailSMSEmailSender = "backend-integration" <> suffix i <> "@wire.com",
                berGalleyJournal = "integration-team-events.fifo" <> suffix i,
                berVHost = mkVHost i,
                berNginzSslPort = mkNginzSslPort i
              }
        )
    & Set.fromList
  where
    suffix :: Word16 -> String
    suffix i = show $ i + 2

    mkNginzSslPort :: Word16 -> Word16
    mkNginzSslPort i = 8443 + ((1 + i) * 1000)

    -- Fixed internal port for federator, e.g. for dynamic backends: 1 -> 10097, 2 -> 11097, etc.
    federatorInternalPort :: Num a => a -> a
    federatorInternalPort i = 8097 + ((1 + i) * 1000)

    -- Fixed external port for federator, e.g. for dynamic backends: 1 -> 10098, 2 -> 11098, etc.
    federatorExternalPort :: Num a => a -> a
    federatorExternalPort i = 8098 + ((1 + i) * 1000)

    -- Fixed domain for a backend resource, e.g. for dynamic backends: 1 -> "c.example.com", 2 -> "d.example.com", etc.
    domain :: Integral a => a -> String
    domain i = [chr (ord 'c' + fromIntegral i - 1)] <> ".example.com"

    mkVHost :: Integral a => a -> String
    mkVHost = domain

remoteDomains :: String -> [String]
remoteDomains domain = ["c.example.com", "d.example.com", "e.example.com"] \\ [domain]
