module V1 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 1 "Add verdict table" $ do
  void $
    schema'
      [r|
        CREATE TABLE if not exists verdict
            ( req                   text
            , format_con            int
            , format_mobile_success text
            , format_mobile_error   text
            , primary key (req)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
