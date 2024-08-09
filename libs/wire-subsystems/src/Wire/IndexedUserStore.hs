{-# LANGUAGE TemplateHaskell #-}

module Wire.IndexedUserStore where

import Database.Bloodhound.Types
import Polysemy
import Wire.UserSearch.Types

data IndexedUserStore m a where
  Upsert :: DocId -> UserDoc -> VersionControl -> IndexedUserStore m ()

makeSem ''IndexedUserStore
