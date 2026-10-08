module Wire.FanInNotificationsAdmin.Postgres where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Imports
import Polysemy
import Wire.FanInNotificationsAdmin
import Wire.Postgres

interpretFanInNotificationsAdminToPostgres :: (PGConstraints r) => InterpreterFor FanInNotificationsAdmin r
interpretFanInNotificationsAdminToPostgres = interpret $ \case
  TruncateAll -> runStatement () truncateAllStatement
  Ping -> runStatement () pingStatement

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
