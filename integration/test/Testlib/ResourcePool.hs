module Testlib.ResourcePool
  ( ResourcePool,
    BackendResource (..),
    backendResources,
    remoteDomains,
    createBackendResourcePool,
    acquireResources,
  )
where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.List.Extra ((\\))
import qualified Data.Set as Set
import Data.String
import Data.Tuple
import Data.Word
import System.IO
import Prelude

data ResourcePool a = ResourcePool
  { sem :: QSemN,
    resources :: IORef (Set.Set a)
  }

acquireResources :: forall m a. (Ord a, MonadIO m, MonadMask m) => Int -> ResourcePool a -> Codensity m [a]
acquireResources n pool = Codensity $ \f -> bracket acquire release (f . Set.toList)
  where
    release :: Set.Set a -> m ()
    release s =
      liftIO $ do
        atomicModifyIORef pool.resources $ (,()) . Set.union s
        signalQSemN pool.sem (length s)

    acquire :: m (Set.Set a)
    acquire = liftIO $ do
      waitQSemN pool.sem n
      atomicModifyIORef pool.resources $ swap . Set.splitAt n

createBackendResourcePool :: IO (ResourcePool BackendResource)
createBackendResourcePool =
  let resources = backendResources 3
   in ResourcePool
        <$> newQSemN 3
        <*> newIORef resources

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

    -- Fixed domain for a backend resource, e.g. for dynamic backends: 1 -> "d1.example.com", 2 -> "d2.example.com", etc.
    domain :: Integral a => a -> String
    domain i = "d" <> show @Int (fromIntegral i) <> ".example.com"

    mkVHost :: Integral a => a -> String
    mkVHost = domain

remoteDomains :: String -> [String]
remoteDomains domain = ["d1.example.com", "d2.example.com", "d3.example.com"] \\ [domain]
