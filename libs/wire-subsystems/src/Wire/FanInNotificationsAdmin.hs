{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin
  ( MigrateResult (..),
    FanInNotificationsAdmin (..),
    migrate,
    truncateAll,
    ping,
    DbName,
    unDbName,
    mkDbName,
    quoteIdent,
  )
where

import Data.Text qualified as T
import Imports
import Polysemy

data MigrateResult = Migrated | AlreadyMigrated
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
  Migrate :: FanInNotificationsAdmin m MigrateResult
  TruncateAll :: FanInNotificationsAdmin m ()
  Ping :: FanInNotificationsAdmin m ()

makeSem ''FanInNotificationsAdmin
