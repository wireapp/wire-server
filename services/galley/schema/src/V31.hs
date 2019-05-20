module V31 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 31 "Add legalhold service table" $ do
    schema' [r|
        CREATE TABLE legalhold_service (
            team_id      uuid,
            base_url     blob,
            fingerprint  blob,
            auth_token   ascii,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
