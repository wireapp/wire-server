module V45 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 45 "Use LeveledCompactionStrategy on prekeys" $
    schema' [r|
        ALTER TABLE prekeys
            WITH compaction = {'class': 'LeveledCompactionStrategy'};
        |]
