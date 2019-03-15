module V31 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 31 "Add team_settings table" $ do
  schema' [r|
      CREATE TABLE team_settings 
        (  team                            uuid    PRIMARY KEY
        ,  user_token_timeout_seconds      bigint
        ,  session_token_timeout_seconds   bigint
        ,  access_token_timeout_seconds    bigint
        ,  provider_token_timeout_seconds  bigint
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
