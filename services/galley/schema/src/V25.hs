module V25 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 25 "Add conversation_codes table" $
    schema'
      [r|
        CREATE TABLE conversation_codes (
            key           ascii, -- opaque conversation ID
            conversation  uuid,
            value         ascii, -- secret value
            scope         int,
            PRIMARY KEY (key, scope)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
