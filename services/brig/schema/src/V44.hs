
module V44 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 44 "Use LeveledCompactionStrategy on clients" $
    schema' [r|
        ALTER TABLE clients
            WITH compaction = {'class': 'LeveledCompactionStrategy'};
        |]
