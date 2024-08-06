{-# LANGUAGE TemplateHaskell #-}

module Wire.IndexedUserStore where

import Data.Id
import Database.Bloodhound.Types
import Polysemy
import Wire.UserSearch.Types

data IndexedUserStore m a where
  Upsert :: DocId -> UserDoc -> VersionControl -> IndexedUserStore m ()
  UpdateTeamSearchVisibilityInbound :: TeamId -> SearchVisibilityInbound -> IndexedUserStore m ()
  -- | Will only be applied to main ES index and not the additional one
  BulkUpsert :: [(DocId, UserDoc, VersionControl)] -> IndexedUserStore m ()

makeSem ''IndexedUserStore
