module Testlib.ResourcePool (ResourcePool, BackendResource (..), backendResources, remoteDomains, createPool) where

import Control.Concurrent (MVar)
import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.List.Extra ((\\))
import qualified Data.Pool
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Word
import System.IO
import Prelude

data ResourcePool = ResourcePool
  { mvar :: MVar (),
    resources :: IORef (Set.Set BackendResource)
  }

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

-------------------------------------------------------------------------
-- resource-pool lib (FUTUREWORK: remove)

createPool = do
  resources <- newIORef $ backendResources 3
  Data.Pool.createPool (create resources) (destroy resources) 1 120 3

destroy :: IORef (Set BackendResource) -> BackendResource -> IO ()
destroy ioRef = modifyIORef' ioRef . Set.insert

create :: IORef (Set.Set BackendResource) -> IO BackendResource
create ioRef =
  atomicModifyIORef
    ioRef
    $ \s ->
      case Set.minView s of
        Nothing -> error "No resources available"
        Just (r, s') -> (s', r)
