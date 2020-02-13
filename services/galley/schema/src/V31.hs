module V31 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 31 "Add legalhold tables" $ do
  schema'
    [r|
        CREATE TABLE legalhold_service (
            team_id      uuid,
            base_url     blob,
            fingerprint  blob,
            auth_token   ascii,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
  schema'
    [r|
        CREATE TABLE legalhold_team_config (
            team_id      uuid,
            status       int,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
  schema'
    [r|
        CREATE TABLE legalhold_pending_prekeys (
            user      uuid,
            key       int,
            data      text,
            PRIMARY KEY (user, key)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
  schema'
    [r|
        CREATE TABLE legalhold_user_status (
                user      uuid,
                status    int,
                PRIMARY KEY (user)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
