{-# LANGUAGE TemplateHaskell #-}

module FanInPerf.NotificationsAdmin.Postgres
  ( interpretFanInNotificationsAdminToPostgres,
    createDatabaseSql,
    dropTablesSql,
    setupPartmanSql,
  )
where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Id
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.UUID (UUID)
import Data.Vector qualified as V
import FanInPerf.NotificationsAdmin
import FanInPerf.NotificationsStore (Stream (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TH
import Imports
import Polysemy
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.Postgres

interpretFanInNotificationsAdminToPostgres :: (PGConstraints r) => InterpreterFor FanInNotificationsAdmin r
interpretFanInNotificationsAdminToPostgres = interpret $ \case
  DropTables -> runSessionWithRetry $ Session.script dropTablesSql
  -- The script is sent as one multi-statement query, which PostgreSQL runs in
  -- a single implicit transaction: either all tables exist or none.
  CreateTables shape -> runSessionWithRetry $ Session.script $ case shape of
    Plain -> commonSchema <> "\n" <> plainNotifications
    Partitioned -> commonSchema <> "\n" <> partitionedNotifications
  CurrentShape -> runStatement () shapeStatement
  CountStreams -> countStreamsImpl
  Ping -> runStatement () pingStatement
  DatabaseExists n -> runStatement n.unDbName databaseExistsStatement
  -- CREATE DATABASE and ALTER SYSTEM cannot run inside a transaction
  -- block, so each is sent as its own single-statement script.
  CreateDatabase n -> runSessionWithRetry $ Session.script (createDatabaseSql n)
  SetupPartman n -> runSessionWithRetry $ traverse_ Session.script (setupPartmanSql n)
  -- like CREATE DATABASE: not inside a transaction block (VACUUM cannot)
  Checkpoint -> runSessionWithRetry $ Session.script "CHECKPOINT"
  VacuumAnalyze -> runSessionWithRetry $ Session.script "VACUUM ANALYZE"
  CountDeadlocks -> fromIntegral <$> runStatement () deadlocksStatement
  CountRows -> fromIntegral <$> runStatement () countRowsStatement
  CountPartitions -> fromIntegral <$> runStatement () countPartitionsStatement
  OldestPartitions -> V.toList <$> runStatement (V.fromList notificationTables) oldestPartitionsStatement
  GetServerInfo -> serverInfoImpl

createDatabaseSql :: DbName -> Text
createDatabaseSql n = "CREATE DATABASE " <> quoteIdent n

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

-- | Not a migration: perf-tool schema in @sql/@. Tables
-- shared by both shapes.
commonSchema :: Text
commonSchema = Text.decodeUtf8 $(makeRelativeToProject "sql/fan-in-common.sql" >>= embedFile)

-- | The six @*_notifications@ tables, plain (baseline).
plainNotifications :: Text
plainNotifications = Text.decodeUtf8 $(makeRelativeToProject "sql/fan-in-notifications-plain.sql" >>= embedFile)

-- | The six @*_notifications@ tables, partitioned and managed by pg_partman.
partitionedNotifications :: Text
partitionedNotifications = Text.decodeUtf8 $(makeRelativeToProject "sql/fan-in-notifications-partitioned.sql" >>= embedFile)

-- | Nothing if the tables are absent, otherwise the shape of @user_notifications@.
shapeStatement :: Statement.Statement () (Maybe SchemaShape)
shapeStatement =
  Statement.unpreparable
    "SELECT c.relkind::text FROM pg_class c WHERE c.oid = to_regclass('user_notifications')"
    Encoders.noParams
    (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable (shapeOf <$> Decoders.text))))
  where
    shapeOf k = if k == T.pack "p" then Partitioned else Plain

-- | Drops the tables, then pg_partman (extension, config, templates), so that
-- 'setupPartman' and @create_parent@ start from scratch. Keep the table list in
-- sync with the @sql/*.sql@ files.
dropTablesSql :: Text
dropTablesSql =
  "DROP TABLE IF EXISTS \
  \user_notifications, client_notifications, team_notifications, \
  \epoch_notifications, local_connection_notifications, remote_connection_notifications, \
  \epoch_history, \
  \last_user_notifications, last_client_notifications, last_team_notifications, \
  \last_epoch_notifications, last_local_connection_notifications, last_remote_connection_notifications, \
  \user_notification_acks, client_notification_acks, team_notification_acks, \
  \epoch_notification_acks, local_connection_acks, remote_connection_acks CASCADE;\n\
  \DROP EXTENSION IF EXISTS pg_partman CASCADE;\n\
  \DROP SCHEMA IF EXISTS partman CASCADE"

-- | Rows per stream. Keep the table list in sync with the
-- @sql/*.sql@ files (connection tables are not read by consumers).
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

deadlocksStatement :: Statement.Statement () Int64
deadlocksStatement =
  [singletonStatement|
    select deadlocks :: int8 from pg_stat_database where datname = current_database()
  |]

-- | Keep the table list in sync with the @sql/*.sql@ files.
countRowsStatement :: Statement.Statement () Int64
countRowsStatement =
  [singletonStatement|
    select (
      (select count(*) from user_notifications)
      + (select count(*) from client_notifications)
      + (select count(*) from team_notifications)
      + (select count(*) from epoch_notifications)
      + (select count(*) from local_connection_notifications)
      + (select count(*) from remote_connection_notifications)
    ) :: int8
  |]

-- | Partitions of all six @*_notifications@ tables (to_regclass is NULL for a
-- missing table, which IN ignores). Keep the table list in sync with the
-- @sql/*.sql@ files.
countPartitionsStatement :: Statement.Statement () Int64
countPartitionsStatement =
  [singletonStatement|
    select count(*) :: int8 from pg_inherits
    where inhparent in (
      to_regclass('user_notifications'),
      to_regclass('client_notifications'),
      to_regclass('team_notifications'),
      to_regclass('epoch_notifications'),
      to_regclass('local_connection_notifications'),
      to_regclass('remote_connection_notifications')
    )
  |]

-- | One row per given table, also for one without partitions.
oldestPartitionsStatement :: Statement.Statement (V.Vector Text) (V.Vector (Text, Text))
oldestPartitionsStatement =
  [vectorStatement|
    select t.name :: text, coalesce(min(c.relname), '') :: text
    from unnest($1 :: text[]) as t(name)
    left join pg_inherits i on i.inhparent = to_regclass(t.name)
    left join pg_class c on c.oid = i.inhrelid
    group by t.name
    order by t.name
  |]

-- | The settings that matter for the numbers of a run.
serverInfoImpl :: (PGConstraints r) => Sem r ServerInfo
serverInfoImpl = do
  version <- runStatement () versionStatement
  settings <- V.toList <$> runStatement () settingsStatement
  partmanVersion <- runStatement () partmanVersionStatement
  pure ServerInfo {version, settings, partmanVersion}

versionStatement :: Statement.Statement () Text
versionStatement =
  [singletonStatement|
    select split_part(version(), ' on ', 1) :: text
  |]

settingsStatement :: Statement.Statement () (V.Vector (Text, Text))
settingsStatement =
  [vectorStatement|
    select name :: text, current_setting(name) :: text
    from pg_settings
    where name in ('shared_buffers', 'max_wal_size', 'synchronous_commit', 'max_connections')
    order by name
  |]

partmanVersionStatement :: Statement.Statement () (Maybe Text)
partmanVersionStatement =
  [maybeStatement|
    select extversion :: text from pg_extension where extname = 'pg_partman'
  |]

pingStatement :: Statement.Statement () ()
pingStatement = Statement.unpreparable "SELECT 1" Encoders.noParams Decoders.noResult
