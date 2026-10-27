{-# LANGUAGE TemplateHaskell #-}

module FanInPerf.NotificationsAdmin
  ( SchemaShape (..),
    FanInNotificationsAdmin (..),
    dropTables,
    createTables,
    currentShape,
    countStreams,
    ping,
    databaseExists,
    createDatabase,
    setupPartman,
    checkpoint,
    vacuumAnalyze,
    countDeadlocks,
    countRows,
    countPartitions,
    getServerInfo,
    ServerInfo (..),
    DbName,
    unDbName,
    mkDbName,
    quoteIdent,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import FanInPerf.NotificationsStore (Stream)
import Imports
import Polysemy

-- | 'Plain' is the production schema; 'Partitioned' replaces the six
-- @*_notifications@ tables by pg_partman managed partitioned tables.
data SchemaShape = Plain | Partitioned
  deriving (Eq, Show)

-- | A validated PostgreSQL database identifier: @^[A-Za-z_][A-Za-z0-9_]{0,62}$@.
-- The constructor is not exported; use 'mkDbName'.
newtype DbName = DbName {unDbName :: Text}
  deriving (Eq, Show)

mkDbName :: Text -> Either Text DbName
mkDbName t = case T.uncons t of
  Just (c, rest)
    | isStart c,
      T.length t <= 63,
      T.all isRest rest ->
        Right (DbName t)
  _ -> Left "invalid database name"
  where
    isStart c = isAsciiLower c || isAsciiUpper c || c == '_'
    isRest c = isStart c || isDigit c

-- | Quotes a validated identifier for use in SQL.
quoteIdent :: DbName -> Text
quoteIdent (DbName n) = "\"" <> n <> "\""

-- | What a result should be read together with: the server version and the
-- settings that matter for the numbers.
data ServerInfo = ServerInfo
  { version :: Text,
    -- | name, value; sorted by name
    settings :: [(Text, Text)],
    -- | 'Nothing' without the extension
    partmanVersion :: Maybe Text
  }
  deriving (Eq, Show)

instance ToJSON ServerInfo where
  toJSON s = object ["version" .= s.version, "settings" .= Map.fromList s.settings, "partman_version" .= s.partmanVersion]

-- | Administrative operations on the fan-in notification tables for tests and
-- performance tooling. Never wire this into services.
data FanInNotificationsAdmin m a where
  -- | Drops all fan-in notification tables (both shapes) and pg_partman, if present.
  DropTables :: FanInNotificationsAdmin m ()
  -- | Creates the fan-in notification tables. Fails if they exist. For
  -- 'Partitioned', 'SetupPartman' must have run before.
  CreateTables :: SchemaShape -> FanInNotificationsAdmin m ()
  -- | Shape of the existing tables, @Nothing@ if absent.
  CurrentShape :: FanInNotificationsAdmin m (Maybe SchemaShape)
  -- | Number of notifications of every stream with at least one row, for user,
  -- client, team and epoch streams (no connection streams).
  CountStreams :: FanInNotificationsAdmin m [(Stream, Int)]
  Ping :: FanInNotificationsAdmin m ()
  -- | Run on the admin connection.
  DatabaseExists :: DbName -> FanInNotificationsAdmin m Bool
  -- | Run on the admin connection.
  CreateDatabase :: DbName -> FanInNotificationsAdmin m ()
  -- | Run on the experiment database; the argument is the database the
  -- pg_partman background worker maintains.
  SetupPartman :: DbName -> FanInNotificationsAdmin m ()
  -- | CHECKPOINT: writes all dirty pages to disk. Run before every measured run
  -- so that each starts at the same point of the checkpoint cycle. Otherwise the
  -- dirty pages of the previous run or of 'DropTables' / 'CreateTables' would be
  -- flushed by a checkpoint at a random time inside the measured phase, and so
  -- would the full-page writes that follow a checkpoint (the first change of a
  -- page after it logs the whole page, which raises the WAL volume and the push
  -- latency for a while). Runs become comparable, and the cost (seconds) is paid
  -- outside the measured phase.
  Checkpoint :: FanInNotificationsAdmin m ()
  -- | VACUUM ANALYZE of the whole database: hint bits, visibility map and
  -- planner statistics are set now and not by the reads of a later run.
  VacuumAnalyze :: FanInNotificationsAdmin m ()
  -- | Deadlocks detected in the database since its statistics were reset
  -- (@pg_stat_database@). Backends report them with a delay of about a second.
  CountDeadlocks :: FanInNotificationsAdmin m Int
  -- | Rows in all six @*_notifications@ tables.
  CountRows :: FanInNotificationsAdmin m Int
  -- | Number of partitions of all six @*_notifications@ tables together (0 if
  -- they are not partitioned): it changes while pg_partman still creates or
  -- drops partitions of any of them.
  CountPartitions :: FanInNotificationsAdmin m Int
  GetServerInfo :: FanInNotificationsAdmin m ServerInfo

makeSem ''FanInNotificationsAdmin
