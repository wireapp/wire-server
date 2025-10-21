module Wire.MigrateConversations where

import Imports
import UnliftIO
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util
import Wire.ConversationStore.Migration

startWorker :: AppT IO CleanupAction
startWorker = do
  cassClient <- asks (.cassandraGalley)
  pgPool <- asks (.hasqlPool)
  logger <- asks (.logger)
  convLoop <- async . lift $ migrateConvsLoop cassClient pgPool logger
  userLoop <- async . lift $ migrateUsersLoop cassClient pgPool logger
  pure $ do
    cancel convLoop
    cancel userLoop
