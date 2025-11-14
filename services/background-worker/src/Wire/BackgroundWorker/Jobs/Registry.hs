-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
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
        . runInputConst (toLocalUnsafe env.federationDomain ())
        . interpretServiceStoreToCassandra env.cassandraBrig
        . interpretUserStoreCassandra env.cassandraBrig
        . interpretUserGroupStoreToPostgres
        . interpretBackgroundJobsPublisherRabbitMQ job.requestId env.amqpJobsPublisherChannel
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
          local = toLocalUnsafe env.federationDomain (),
          requestId = job.requestId
        }

interpretTinyLog ::
  (Member (Embed IO) r) =>
  Env ->
  RequestId ->
  JobId ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog e reqId jobId =
  loggerToTinyLog e.logger
    . mapLogger ((field "request" (unRequestId reqId) . field "job" (idToText jobId)) .)
    . raiseUnder @P.TinyLog
