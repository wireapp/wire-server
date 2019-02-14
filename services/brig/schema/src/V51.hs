
module V51 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 51 "Add blacklist table" $
    schema' [r|
        create table if not exists blacklist
            ( key      text      -- blacklisted email/phone
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
