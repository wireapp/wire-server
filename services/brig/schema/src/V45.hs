module V45 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 45 "Use LeveledCompactionStrategy on prekeys" $
    schema'
      [r|
        ALTER TABLE prekeys
            WITH compaction = {'class': 'LeveledCompactionStrategy'};
        |]
