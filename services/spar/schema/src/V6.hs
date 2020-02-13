{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module V6
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 6 "Store raw XML metadata" $ do
  void $
    schema'
      [r|
        CREATE TABLE if not exists idp_raw_metadata
            ( id       uuid
            , metadata text
            , primary key (id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
