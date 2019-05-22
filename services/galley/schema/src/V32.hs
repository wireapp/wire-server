module V32 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 32 "Add legalhold team configuration table" $ do
    schema' [r|
        CREATE TABLE legalhold_team_config (
            team_id      uuid,
            status       int,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
