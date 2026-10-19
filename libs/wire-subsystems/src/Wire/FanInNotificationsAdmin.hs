{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin
  ( SchemaShape (..),
    MigrateResult (..),
    FanInNotificationsAdmin (..),
    migrate,
    truncateAll,
    ping,
    databaseExists,
    createDatabase,
    dropDatabase,
    setupPartman,
    DbName,
    unDbName,
    mkDbName,
    quoteIdent,
  )
where

import Data.Text qualified as T
import Imports
import Polysemy

-- | 'Plain' is the production schema; 'Partitioned' replaces the six
-- @*_notifications@ tables by pg_partman managed partitioned tables.
data SchemaShape = Plain | Partitioned
  deriving (Eq, Show)

data MigrateResult = Migrated | AlreadyMigrated | ExistsWithOtherShape SchemaShape
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

-- | Administrative operations on the fan-in notification tables for tests and
-- performance tooling. Never wire this into services.
data FanInNotificationsAdmin m a where
  -- | Creates the fan-in notification tables; skipped if they already exist.
  Migrate :: SchemaShape -> FanInNotificationsAdmin m MigrateResult
  TruncateAll :: FanInNotificationsAdmin m ()
  Ping :: FanInNotificationsAdmin m ()
  -- | Run on the admin connection.
  DatabaseExists :: DbName -> FanInNotificationsAdmin m Bool
  -- | Run on the admin connection.
  CreateDatabase :: DbName -> FanInNotificationsAdmin m ()
  -- | Run on the admin connection. @IF EXISTS@, @WITH (FORCE)@.
  DropDatabase :: DbName -> FanInNotificationsAdmin m ()
  -- | Run on the experiment database; the argument is the database the
  -- pg_partman background worker maintains.
  SetupPartman :: DbName -> FanInNotificationsAdmin m ()

makeSem ''FanInNotificationsAdmin
