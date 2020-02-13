module V40 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 40 "Add hashed userkeys table" $
    schema'
      [r|
        create table if not exists user_keys_hash
            ( key      blob
            , key_type int  -- hash type (0 = PHONE, 1 = EMAIL)
            , user     uuid
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
