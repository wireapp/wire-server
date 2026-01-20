-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MigrateConversationCodes where

import Imports
import Prometheus
import System.Logger qualified as Log
import UnliftIO
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util
import Wire.CodeStore.Migration

startWorker :: MigrationOptions -> AppT IO CleanupAction
startWorker migOpts = do
  cassClient <- asks (.cassandraGalley)
  pgPool <- asks (.hasqlPool)
  logger <- asks (.logger)
  Log.info logger $ Log.msg (Log.val "starting conversation codes migration")
  count <- register $ counter $ Prometheus.Info "wire_conv_codes_migrated_to_pg" "Number of conversation codes migrated to Postgresql"
  finished <- register $ counter $ Prometheus.Info "wire_conv_codes_migration_finished" "Whether the conversation codes migration to Postgresql is finished successfully"
  failed <- register $ counter $ Prometheus.Info "wire_conv_codes_migration_failed" "Whether the conversation codes migration to Postgresql has failed"

  migrationLoop <- async . lift $ migrateCodesLoop migOpts cassClient pgPool logger count finished failed

  Log.info logger $ Log.msg (Log.val "started conversation codes migration")
  pure $ do
    Log.info logger $ Log.msg (Log.val "cancelling conversation codes migration")
    cancel migrationLoop
