module V33 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 33 "Add LegalHold pending prekeys table" $ do
    schema' [r|
        CREATE TABLE legalhold_pending_prekeys (
                user      uuid,
                key       int,
                data      text
        );
        PRIMARY KEY (user, key)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
