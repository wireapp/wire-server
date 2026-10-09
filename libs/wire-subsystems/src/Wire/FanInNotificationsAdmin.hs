{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin where

import Imports
import Polysemy

data MigrateResult = Migrated | AlreadyMigrated
  deriving (Eq, Show)

-- | Administrative operations on the fan-in notification tables for tests and
-- performance tooling. Never wire this into services.
data FanInNotificationsAdmin m a where
  -- | Creates the fan-in notification tables; skipped if they already exist.
  Migrate :: FanInNotificationsAdmin m MigrateResult
  TruncateAll :: FanInNotificationsAdmin m ()
  Ping :: FanInNotificationsAdmin m ()

makeSem ''FanInNotificationsAdmin
