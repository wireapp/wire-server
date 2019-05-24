module V33 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 33 "Add LegalHold user status table" $ do
    schema' [r|
        CREATE TABLE legalhold_user_status (
                user      uuid,
                status    int
        );
        PRIMARY KEY (user)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
