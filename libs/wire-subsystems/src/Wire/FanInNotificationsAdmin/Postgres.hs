{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin.Postgres
  ( interpretFanInNotificationsAdminToPostgres,
    createDatabaseSql,
    dropDatabaseSql,
    setupPartmanSql,
  )
where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Id
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.UUID (UUID)
import Data.Vector qualified as V
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TH
import Imports
import Polysemy
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.FanInNotificationsAdmin
import Wire.FanInNotificationsStore (Stream (..))
import Wire.Postgres

interpretFanInNotificationsAdminToPostgres :: (PGConstraints r) => InterpreterFor FanInNotificationsAdmin r
interpretFanInNotificationsAdminToPostgres = interpret $ \case
  -- The script is sent as one multi-statement query, which PostgreSQL runs in
  -- a single implicit transaction: either all tables exist or none.
  Migrate shape -> runSessionWithRetry $ do
    existing <- Session.statement () shapeStatement
    case existing of
      Nothing -> do
        Session.script $ case shape of
          Plain -> commonSchema <> "\n" <> plainNotifications
          Partitioned -> commonSchema <> "\n" <> partitionedNotifications
        pure Migrated
      Just s
        | s == shape -> pure AlreadyMigrated
        | otherwise -> pure (ExistsWithOtherShape s)
  CurrentShape -> runStatement () shapeStatement
  TruncateAll -> runStatement () truncateAllStatement
  CountStreams -> countStreamsImpl
  Ping -> runStatement () pingStatement
  DatabaseExists n -> runStatement n.unDbName databaseExistsStatement
  -- CREATE/DROP DATABASE and ALTER SYSTEM cannot run inside a transaction
  -- block, so each is sent as its own single-statement script.
  CreateDatabase n -> runSessionWithRetry $ Session.script (createDatabaseSql n)
  DropDatabase n -> runSessionWithRetry $ Session.script (dropDatabaseSql n)
  SetupPartman n -> runSessionWithRetry $ traverse_ Session.script (setupPartmanSql n)

createDatabaseSql :: DbName -> Text
createDatabaseSql n = "CREATE DATABASE " <> quoteIdent n

dropDatabaseSql :: DbName -> Text
dropDatabaseSql n = "DROP DATABASE IF EXISTS " <> quoteIdent n <> " WITH (FORCE)"

-- | Statements to run one by one on the experiment database. The database name
-- is rendered as a string literal; this is safe because 'DbName' is
-- whitelisted (no quotes possible).
setupPartmanSql :: DbName -> [Text]
setupPartmanSql n =
  [ "CREATE SCHEMA IF NOT EXISTS partman",
    "CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA partman",
    "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'partman_maintainer') THEN CREATE ROLE partman_maintainer WITH LOGIN; END IF; END $$",
    "GRANT ALL ON SCHEMA partman TO partman_maintainer",
    "GRANT ALL ON ALL TABLES IN SCHEMA partman TO partman_maintainer",
    "GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA partman TO partman_maintainer",
    "GRANT EXECUTE ON ALL PROCEDURES IN SCHEMA partman TO partman_maintainer",
    "GRANT CREATE, USAGE ON SCHEMA public TO partman_maintainer",
    "GRANT TEMPORARY ON DATABASE " <> quoteIdent n <> " TO partman_maintainer",
    "ALTER SYSTEM SET pg_partman_bgw.dbname = '" <> n.unDbName <> "'",
    "ALTER SYSTEM SET pg_partman_bgw.interval = 60",
    "ALTER SYSTEM SET pg_partman_bgw.role = 'partman_maintainer'",
    "SELECT pg_reload_conf()"
  ]

databaseExistsStatement :: Statement.Statement Text Bool
databaseExistsStatement =
  Statement.preparable
    "SELECT EXISTS (SELECT 1 FROM pg_database WHERE datname = $1)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

-- | Not a migration: the fan-in SQL files are only used by fan-in-perf. Tables
-- shared by both shapes.
commonSchema :: Text
commonSchema = Text.decodeUtf8 $(makeRelativeToProject "fan-in-sql/fan-in-common.sql" >>= embedFile)

-- | The six @*_notifications@ tables, plain (baseline).
plainNotifications :: Text
plainNotifications = Text.decodeUtf8 $(makeRelativeToProject "fan-in-sql/fan-in-notifications-plain.sql" >>= embedFile)

-- | The six @*_notifications@ tables, partitioned and managed by pg_partman.
partitionedNotifications :: Text
partitionedNotifications = Text.decodeUtf8 $(makeRelativeToProject "fan-in-sql/fan-in-notifications-partitioned.sql" >>= embedFile)

-- | Nothing if the tables are absent, otherwise the shape of @user_notifications@.
shapeStatement :: Statement.Statement () (Maybe SchemaShape)
shapeStatement =
  Statement.unpreparable
    "SELECT c.relkind::text FROM pg_class c WHERE c.oid = to_regclass('user_notifications')"
    Encoders.noParams
    (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable (shapeOf <$> Decoders.text))))
  where
    shapeOf k = if k == T.pack "p" then Partitioned else Plain

-- | Keep the table list in sync with the @fan-in-sql/*.sql@ files.
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

-- | Rows per stream. Keep the table list in sync with the
-- @fan-in-sql/*.sql@ files (connection tables are not read by consumers).
countStreamsImpl :: (PGConstraints r) => Sem r [(Stream, Int)]
countStreamsImpl = do
  users <- runStatement () countUserStatement
  clients <- runStatement () countClientStatement
  teams <- runStatement () countTeamStatement
  epochs <- runStatement () countEpochStatement
  pure . concat $
    [ [(StreamUser (Id u), fromIntegral n) | (u, n) <- V.toList users],
      [(StreamClient (Id u) (ClientId (fromIntegral c)), fromIntegral n) | (u, c, n) <- V.toList clients],
      [(StreamTeam (Id t), fromIntegral n) | (t, n) <- V.toList teams],
      [(StreamEpoch (GroupId g) (Epoch (fromIntegral e)), fromIntegral n) | (g, e, n) <- V.toList epochs]
    ]

countUserStatement :: Statement.Statement () (V.Vector (UUID, Int64))
countUserStatement =
  [vectorStatement|
    select user_id :: uuid, count(*) :: int8 from user_notifications group by user_id
  |]

countClientStatement :: Statement.Statement () (V.Vector (UUID, Int64, Int64))
countClientStatement =
  [vectorStatement|
    select user_id :: uuid, client_id :: int8, count(*) :: int8
    from client_notifications group by user_id, client_id
  |]

countTeamStatement :: Statement.Statement () (V.Vector (UUID, Int64))
countTeamStatement =
  [vectorStatement|
    select team_id :: uuid, count(*) :: int8 from team_notifications group by team_id
  |]

countEpochStatement :: Statement.Statement () (V.Vector (ByteString, Int64, Int64))
countEpochStatement =
  [vectorStatement|
    select group_id :: bytea, epoch :: int8, count(*) :: int8
    from epoch_notifications group by group_id, epoch
  |]

pingStatement :: Statement.Statement () ()
pingStatement = Statement.unpreparable "SELECT 1" Encoders.noParams Decoders.noResult
