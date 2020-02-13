module V37 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 37 "Add budget table" $
    schema'
      [r|
        create table if not exists budget
            ( key    text
            , budget int
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'}
              and gc_grace_seconds = 0;
    |]
