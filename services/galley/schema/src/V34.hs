module V34 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 34 "Add team features table" $ do
    schema' [r|
        CREATE TABLE team_features (
            team_id          uuid,
            legalhold_status int,
            sso_status       int,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
