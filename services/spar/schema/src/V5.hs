{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V5 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 5 "Store SCIM user blobs" $ do
    void $ schema' [r|
        CREATE TABLE if not exists scim_user
            ( uid    uuid
            , json   blob
            , PRIMARY KEY (uid)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
