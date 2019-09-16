{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V5 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 5 "Store SCIM user blobs" $ do
    -- docs/developer/scim/storage.md {#DevScimStorageUsers}
    void $ schema' [r|
        CREATE TABLE if not exists scim_user
            ( id     uuid
            , json   blob
            , PRIMARY KEY (id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
