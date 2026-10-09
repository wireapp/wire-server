{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin.Postgres where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text.Encoding qualified as Text
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Imports
import Polysemy
import Wire.FanInNotificationsAdmin
import Wire.Postgres

interpretFanInNotificationsAdminToPostgres :: (PGConstraints r) => InterpreterFor FanInNotificationsAdmin r
interpretFanInNotificationsAdminToPostgres = interpret $ \case
  -- The script is sent as one multi-statement query, which PostgreSQL runs in
  -- a single implicit transaction: either all tables exist or none.
  Migrate -> runSessionWithRetry $ do
    exists <- Session.statement () schemaExistsStatement
    if exists
      then pure AlreadyMigrated
      else Session.script fanInSchema $> Migrated
  TruncateAll -> runStatement () truncateAllStatement
  Ping -> runStatement () pingStatement

-- | Run as plain script, without hasql-migration bookkeeping.
fanInSchema :: Text
fanInSchema = Text.decodeUtf8 $(makeRelativeToProject "postgres-migrations/20260729073800-fan-in-notifications.sql" >>= embedFile)

schemaExistsStatement :: Statement.Statement () Bool
schemaExistsStatement =
  Statement.unpreparable
    "SELECT to_regclass('user_notifications') IS NOT NULL"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

-- | Keep the table list in sync with
-- @postgres-migrations/20260729073800-fan-in-notifications.sql@.
truncateAllStatement :: Statement.Statement () ()
truncateAllStatement =
  Statement.unpreparable
    "TRUNCATE TABLE \
    \user_notifications, client_notifications, team_notifications, \
    \epoch_notifications, local_connection_notifications, remote_connection_notifications, \
    \epoch_history, \
    \last_user_notifications, last_client_notifications, last_team_notifications, \
    \last_epoch_notifications, last_local_connection_notifications, last_remote_connection_notifications, \
    \user_notification_acks, client_notification_acks, team_notification_acks, \
    \epoch_notification_acks, local_connection_acks, remote_connection_acks"
    Encoders.noParams
    Decoders.noResult

pingStatement :: Statement.Statement () ()
pingStatement = Statement.unpreparable "SELECT 1" Encoders.noParams Decoders.noResult
