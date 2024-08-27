{-# LANGUAGE TemplateHaskell #-}

module Wire.IndexedUserStore.Bulk where

import Polysemy

-- | Bulk operations, must not be used from any web handler
data IndexedUserStoreBulk m a where
  -- | Only changes data if it is not updated since last update, use when users
  -- need to be synced because of an outage, or migrating to a new ES instance.
  SyncAllUsers :: IndexedUserStoreBulk m ()
  -- | Overwrite all users in the ES index, use it when trying to fix some
  -- inconsistency or while introducing a new field in the mapping.
  ForceSyncAllUsers :: IndexedUserStoreBulk m ()
  MigrateData :: IndexedUserStoreBulk m ()

makeSem ''IndexedUserStoreBulk
