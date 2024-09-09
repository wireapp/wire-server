module Testlib.ResourcePool
  ( ResourcePool,
    BackendResource (..),
    DynamicBackendConfig (..),
    resourceServiceMap,
    backendResources,
    createBackendResourcePool,
    acquireResources,
    backendA,
    backendB,
  )
where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable (for_)
import Data.Functor
import Data.IORef
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import Data.Tuple
import Database.CQL.IO
import GHC.Stack (HasCallStack)
import Network.AMQP.Extended
import Network.RabbitMqAdmin
import System.IO
import qualified Testlib.Ports as Ports
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
          spar = g Spar,
          -- FUTUREWORK: Set to g Proxy, when we add Proxy to spawned services
          proxy = HostPort "127.0.0.1" 9087,
          stern = g Stern
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

createBackendResourcePool :: [BackendResource] -> RabbitMQConfig -> ClientState -> IO (ResourcePool BackendResource)
createBackendResourcePool resources rabbitmq cassClient =
  let cleanupBackend :: BackendResource -> IO ()
      cleanupBackend resource = do
        deleteAllRabbitMQQueues rabbitmq resource
        runClient cassClient $ deleteAllDynamicBackendConfigs resource
   in ResourcePool
        <$> newQSemN (length resources)
        <*> newIORef (Set.fromList resources)
        <*> pure cleanupBackend

deleteAllRabbitMQQueues :: RabbitMQConfig -> BackendResource -> IO ()
deleteAllRabbitMQQueues rc resource = do
  let opts =
        RabbitMqAdminOpts
          { host = rc.host,
            port = 0,
            adminPort = fromIntegral rc.adminPort,
            vHost = T.pack resource.berVHost
          }
  client <- mkRabbitMqAdminClientEnv opts
  queues <- listQueuesByVHost client (T.pack resource.berVHost)
  for_ queues $ \queue ->
    deleteQueue client (T.pack resource.berVHost) queue.name

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
                    berFederatorInternal = Ports.portForDyn (Ports.ServiceInternal FederatorInternal) i,
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
                    berNginzSslPort = Ports.portForDyn Ports.NginzSSL i,
                    berNginzHttp2Port = Ports.portForDyn Ports.NginzHttp2 i,
                    berInternalServicePorts = Ports.internalServicePorts name,
                    berMlsPrivateKeyPaths = dynConf.mlsPrivateKeyPaths
                  }
        )
  where
    suffix :: (Show a, Num a) => a -> String
    suffix i = show $ i + 2

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
      berInternalServicePorts = Ports.internalServicePorts BackendA,
      berNginzHttp2Port = Ports.port Ports.NginzHttp2 BackendA,
      berMlsPrivateKeyPaths =
        object
          [ fromString "removal"
              .= object
                [ fromString "ed25519" .= "test/resources/backendA/ed25519.pem",
                  fromString "ecdsa_secp256r1_sha256" .= "test/resources/backendA/ecdsa_secp256r1_sha256.pem",
                  fromString "ecdsa_secp384r1_sha384" .= "test/resources/backendA/ecdsa_secp384r1_sha384.pem",
                  fromString "ecdsa_secp521r1_sha512" .= "test/resources/backendA/ecdsa_secp521r1_sha512.pem"
                ]
          ]
    }

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
      berAwsUserJournalQueue = "integration-user-events2.fifo",
      berAwsPrekeyTable = "integration-brig-prekeys2",
      berAwsS3Bucket = "dummy-bucket2",
      berAwsQueueName = "integration-gundeck-events2",
      berBrigInternalEvents = "integration-brig-events-internal2",
      berEmailSMSSesQueue = "integration-brig-events2",
      berEmailSMSEmailSender = "backend-integration2@wire.com",
      berGalleyJournal = "integration-team-events2.fifo",
      -- FUTUREWORK: set up vhosts in dev/ci for example.com and b.example.com
      -- in case we want backendA and backendB to federate with a third backend
      -- (because otherwise both queues will overlap)
      berVHost = "backendB",
      berNginzSslPort = Ports.port Ports.NginzSSL BackendB,
      berInternalServicePorts = Ports.internalServicePorts BackendB,
      berNginzHttp2Port = Ports.port Ports.NginzHttp2 BackendB,
      berMlsPrivateKeyPaths =
        object
          [ fromString "removal"
              .= object
                [ fromString "ed25519" .= "test/resources/backendB/ed25519.pem",
                  fromString "ecdsa_secp256r1_sha256" .= "test/resources/backendB/ecdsa_secp256r1_sha256.pem",
                  fromString "ecdsa_secp384r1_sha384" .= "test/resources/backendB/ecdsa_secp384r1_sha384.pem",
                  fromString "ecdsa_secp521r1_sha512" .= "test/resources/backendB/ecdsa_secp521r1_sha512.pem"
                ]
          ]
    }
