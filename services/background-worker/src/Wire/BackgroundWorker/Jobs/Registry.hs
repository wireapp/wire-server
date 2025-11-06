module Wire.BackgroundWorker.Jobs.Registry
  ( dispatchJob,
  )
where

import Data.Id
import Data.Qualified
import Data.Text qualified as T
import Hasql.Pool (UsageError)
import Imports
import Polysemy
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger as Logger
import Wire.API.BackgroundJobs (Job (..))
import Wire.API.Federation.Error (FederationError)
import Wire.BackendNotificationQueueAccess.RabbitMq qualified as BackendNotificationQueueAccess
import Wire.BackgroundJobsPublisher.RabbitMQ (interpretBackgroundJobsPublisherRabbitMQ)
import Wire.BackgroundJobsRunner (runJob)
import Wire.BackgroundJobsRunner.Interpreter hiding (runJob)
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.BrigAPIAccess.Rpc
import Wire.ConversationStore
import Wire.ConversationStore.Cassandra
import Wire.ConversationStore.Postgres (interpretConversationStoreToPostgres)
import Wire.ConversationSubsystem.Interpreter (interpretConversationSubsystem)
import Wire.ExternalAccess.External
import Wire.FireAndForget (interpretFireAndForget)
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem.Interpreter
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Delay (runDelay)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random.IO (randomToIO)
import Wire.ServiceStore.Cassandra (interpretServiceStoreToCassandra)
import Wire.UserGroupStore.Postgres (interpretUserGroupStoreToPostgres)
import Wire.UserStore.Cassandra (interpretUserStoreCassandra)

dispatchJob :: Job -> AppT IO (Either Text ())
dispatchJob job = do
  env <- ask @Env
  let disableTlsV1 = True
  extEnv <- liftIO (initExtEnv disableTlsV1)
  liftIO $ runInterpreters env extEnv $ runJob job
  where
    convStoreInterpreter env =
      case env.postgresMigration.conversation of
        CassandraStorage -> interpretConversationStoreToCassandra env.cassandraGalley
        MigrationToPostgresql -> interpretConversationStoreToCassandraAndPostgres env.cassandraGalley
        PostgresqlStorage -> interpretConversationStoreToPostgres
    runInterpreters env extEnv = do
      runFinal @IO
        . embedToFinal @IO
        . asyncToIOFinal
        . interpretRace
        . runDelay
        . runError
        . mapError @FederationError (T.pack . show)
        . mapError @UsageError (T.pack . show)
        . mapError @ParseException (T.pack . show)
        . mapError @MigrationError (T.pack . show)
        . interpretTinyLog env job.requestId job.jobId
        . runInputConst env.hasqlPool
        . runInputConst (toLocalUnsafe env.domain ())
        . interpretServiceStoreToCassandra env.cassandraBrig
        . interpretUserStoreCassandra env.cassandraBrig
        . interpretUserGroupStoreToPostgres
        . runInputSem (readMVar env.amqpJobsPublisherChannel)
        . interpretBackgroundJobsPublisherRabbitMQ job.requestId
        . nowToIO
        . randomToIO
        . interpretFireAndForget
        . BackendNotificationQueueAccess.interpretBackendNotificationQueueAccess (Just $ backendQueueEnv env)
        . convStoreInterpreter env
        . runRpcWithHttp env.httpManager job.requestId
        . runGundeckAPIAccess env.gundeckEndpoint
        -- FUTUREWORK: Currently the brig access effect is needed for the interpreter of ExternalAccess.
        -- At the time of implementation the only function used from ExternalAccess is deliverAsync, which will not call brig access.
        -- However, to prevent the background worker to require HTTP access to brig, we should consider refactoring this at some point.
        . interpretBrigAccess env.brigEndpoint
        . interpretExternalAccess extEnv
        . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig job.requestId)
        . interpretConversationSubsystem
        . interpretBackgroundJobsRunner

    backendQueueEnv env =
      BackendNotificationQueueAccess.Env
        { channelMVar = env.amqpBackendNotificationsChannel,
          logger = env.logger,
          local = toLocalUnsafe env.domain (),
          requestId = job.requestId
        }

interpretTinyLog ::
  (Member (Embed IO) r) =>
  Env ->
  RequestId ->
  JobId ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog e reqId jobId = interpret $ \case
  P.Log l m -> Logger.log e.logger l ((("request" .=) . unRequestId) reqId . (("job" .=) . idToText) jobId . m)
