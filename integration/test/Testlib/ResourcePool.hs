{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Testlib.ResourcePool
  ( ResourcePool,
    BackendResource (..),
    DynamicBackendConfig (..),
    resourceServiceMap,
    backendResources,
    createBackendResourcePool,
    acquireResources,
  )
where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Functor
import Data.IORef
import qualified Data.Set as Set
import Data.String
import Data.Tuple
import Database.CQL.IO
import GHC.Stack (HasCallStack)
import Network.AMQP.Extended
import Network.RabbitMqAdmin
import System.IO
import Testlib.Ports
import Testlib.Types
import Prelude

resourceServiceMap :: BackendResource -> ServiceMap
resourceServiceMap resource =
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
          proxy = g WireProxy,
          spar = g Spar,
          stern = g Stern,
          wireServerEnterprise = g WireServerEnterprise,
          rabbitMqVHost = fromString resource.berVHost
        }

acquireResources :: forall m a. (Ord a, MonadIO m, MonadMask m, HasCallStack) => Int -> ResourcePool a -> Codensity m [a]
acquireResources n pool = Codensity $ \f -> bracket acquire release $ \s -> do
  liftIO $ mapM_ pool.onAcquire s
  f $ Set.toList s
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

createBackendResourcePool :: [BackendResource] -> RabbitMqAdminOpts -> ClientState -> IO (ResourcePool BackendResource)
createBackendResourcePool resources rabbitmq cassClient =
  let cleanupBackend :: BackendResource -> IO ()
      cleanupBackend resource = do
        deleteAllRabbitMQQueues rabbitmq resource
        runClient cassClient $ deleteAllDynamicBackendConfigs resource
   in ResourcePool
        <$> newQSemN (length resources)
        <*> newIORef (Set.fromList resources)
        <*> pure cleanupBackend

deleteAllRabbitMQQueues :: RabbitMqAdminOpts -> BackendResource -> IO ()
deleteAllRabbitMQQueues opts resource = do
  client <- mkRabbitMqAdminClientEnv opts {vHost = fromString resource.berVHost}
  queuesPage <- listQueuesByVHost client (fromString resource.berVHost) (fromString "") False 100 1
  for_ queuesPage.items $ \queue ->
    deleteQueue client (fromString resource.berVHost) queue.name

deleteAllDynamicBackendConfigs :: BackendResource -> Client ()
deleteAllDynamicBackendConfigs resource = write cql (defQueryParams LocalQuorum ())
  where
    cql :: PrepQuery W () ()
    cql = fromString $ "TRUNCATE " <> resource.berBrigKeyspace <> ".federation_remotes"

backendResources :: [DynamicBackendConfig] -> [BackendResource]
backendResources dynConfs =
  (zip dynConfs [1 ..])
    <&> ( \(dynConf, i) ->
            let name = DynamicBackend i
             in BackendResource
                  { berName = name,
                    berBrigKeyspace = "brig_test_dyn_" <> show i,
                    berGalleyKeyspace = "galley_test_dyn_" <> show i,
                    berSparKeyspace = "spar_test_dyn_" <> show i,
                    berGundeckKeyspace = "gundeck_test_dyn_" <> show i,
                    berElasticsearchIndex = "directory_dyn_" <> show i <> "_test",
                    berFederatorInternal = portForDyn (ServiceInternal FederatorInternal) i,
                    berFederatorExternal = dynConf.federatorExternalPort,
                    berDomain = dynConf.domain,
                    berAwsUserJournalQueue = "integration-user-events" <> suffix i <> ".fifo",
                    berAwsPrekeyTable = "integration-brig-prekeys" <> suffix i,
                    berAwsS3Bucket = "dummy-bucket" <> suffix i,
                    berAwsQueueName = "integration-gundeck-events" <> suffix i,
                    berBrigInternalEvents = "integration-brig-events-internal" <> suffix i,
                    berEmailSMSSesQueue = "integration-brig-events" <> suffix i,
                    berEmailSMSEmailSender = "backend-integration" <> suffix i <> "@wire.com",
                    berGalleyJournal = "integration-team-events" <> suffix i <> ".fifo",
                    berVHost = dynConf.domain,
                    berNginzSslPort = portForDyn NginzSSL i,
                    berNginzHttp2Port = portForDyn NginzHttp2 i,
                    berInternalServicePorts = internalServicePorts name,
                    berEnableService = const True,
                    berMlsPrivateKeyPaths = dynConf.mlsPrivateKeyPaths
                  }
        )
  where
    suffix :: (Show a, Num a) => a -> String
    suffix i = show $ i + 2
