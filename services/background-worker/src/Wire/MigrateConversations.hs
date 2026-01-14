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

module Wire.MigrateConversations where

import Imports
import Prometheus
import System.Logger qualified as Log
import UnliftIO
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util
import Wire.ConversationStore.Migration

startWorker :: MigrationOptions -> AppT IO CleanupAction
startWorker migOpts = do
  cassClient <- asks (.cassandraGalley)
  pgPool <- asks (.hasqlPool)
  logger <- asks (.logger)

  Log.info logger $ Log.msg (Log.val "starting conversation migration")
  convMigCounter <- register $ counter $ Prometheus.Info "wire_local_convs_migrated_to_pg" "Number of local conversations migrated to Postgresql"
  convMigFinished <- register $ counter $ Prometheus.Info "wire_local_convs_migration_finished" "Whether the conversation migration to Postgresql is finished successfully"
  convMigFailed <- register $ counter $ Prometheus.Info "wire_local_convs_migration_failed" "Whether the conversation migration to Postgresql has failed"
  userMigCounter <- register $ counter $ Prometheus.Info "wire_user_remote_convs_migrated_to_pg" "Number of users whose remote conversation membership data is migrated to Postgresql"
  userMigFinished <- register $ counter $ Prometheus.Info "wire_user_remote_convs_migration_finished" "Whether the migration of remote conversation membership data to Postgresql is finished successfully"
  userMigFailed <- register $ counter $ Prometheus.Info "wire_user_remote_convs_migration_failed" "Whether the migration of remote conversation membership data to Postgresql has failed"

  convLoop <- async . lift $ migrateConvsLoop migOpts cassClient pgPool logger convMigCounter convMigFinished convMigFailed
  userLoop <- async . lift $ migrateUsersLoop migOpts cassClient pgPool logger userMigCounter userMigFinished userMigFailed

  Log.info logger $ Log.msg (Log.val "started conversation migration")
  pure $ do
    Log.info logger $ Log.msg (Log.val "cancelling conversation migration")
    cancel convLoop
    cancel userLoop
