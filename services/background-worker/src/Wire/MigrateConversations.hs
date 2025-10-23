module Wire.MigrateConversations where

import Imports
import Prometheus
import System.Logger qualified as Log
import UnliftIO
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util
import Wire.ConversationStore.Migration

startWorker :: AppT IO CleanupAction
startWorker = do
  cassClient <- asks (.cassandraGalley)
  pgPool <- asks (.hasqlPool)
  logger <- asks (.logger)

  Log.info logger $ Log.msg (Log.val "starting conversation migration")
  convMigCounter <- register $ counter $ Prometheus.Info "wire_local_convs_migrated_to_pg" "Number of local conversations migrated to Postgresql"
  convMigFinished <- register $ counter $ Prometheus.Info "wire_local_convs_migration_finished" "Whether the converastion migateion to Postgresql is finished"
  userMigCounter <- register $ counter $ Prometheus.Info "wire_user_remote_convs_migrated_to_pg" "Number of users whose remote conversation membership data is migrated to Postgresql"
  userMigFinished <- register $ counter $ Prometheus.Info "wire_user_remote_convs_migration_finished" "Whether the migration of remote conversation membership data to Postgresql is finished"

  convLoop <- async . lift $ migrateConvsLoop cassClient pgPool logger convMigCounter convMigFinished
  userLoop <- async . lift $ migrateUsersLoop cassClient pgPool logger userMigCounter userMigFinished

  Log.info logger $ Log.msg (Log.val "started conversation migration")
  pure $ do
    Log.info logger $ Log.msg (Log.val "cancelling conversation migration")
    cancel convLoop
    cancel userLoop
