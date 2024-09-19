{-# LANGUAGE TemplateHaskell #-}

module Wire.IndexedUserStore.MigrationStore where

import Polysemy
import Wire.UserSearch.Migration

data IndexedUserMigrationStore m a where
  EnsureMigrationIndex :: IndexedUserMigrationStore m ()
  GetLatestMigrationVersion :: IndexedUserMigrationStore m MigrationVersion
  PersistMigrationVersion :: MigrationVersion -> IndexedUserMigrationStore m ()

makeSem ''IndexedUserMigrationStore
