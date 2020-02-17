module V44 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 44 "Use LeveledCompactionStrategy on clients" $
    schema'
      [r|
        ALTER TABLE clients
            WITH compaction = {'class': 'LeveledCompactionStrategy'};
        |]
