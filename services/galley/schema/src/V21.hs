module V21 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 21 "Add teams" $ do
  schema'
    [r|
        CREATE TABLE team (
            team     uuid PRIMARY KEY,
            creator  uuid,
            name     text,
            icon     text,
            icon_key text,
            deleted  boolean
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema'
    [r|
        CREATE TABLE team_conv (
            team    uuid,
            conv    uuid,
            managed boolean,
            PRIMARY KEY (team, conv)
        ) WITH CLUSTERING ORDER BY (conv ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema' [r| CREATE TYPE permissions (self bigint, copy bigint); |]
  schema'
    [r|
        CREATE TABLE team_member (
            team  uuid,
            user  uuid,
            perms frozen<permissions>,
            PRIMARY KEY (team, user)
        ) WITH CLUSTERING ORDER BY (user ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema'
    [r|
        CREATE TABLE user_team (
            user  uuid,
            team  uuid,
            PRIMARY KEY (user, team)
        ) WITH CLUSTERING ORDER BY (team ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema' [r| ALTER TABLE conversation ADD team uuid; |]
  schema' [r| ALTER TABLE conversation ADD deleted boolean; |]
