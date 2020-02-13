{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module V5
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 5 "Store SCIM user blobs" $ do
  -- docs/developer/scim/storage.md {#DevScimStorageUsers}
  void $
    schema'
      [r|
        CREATE TABLE if not exists scim_user
            ( id     uuid
            , json   blob
            , PRIMARY KEY (id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
