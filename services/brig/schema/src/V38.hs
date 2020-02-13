module V38 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 38 "Add user handles" $ do
  schema'
    [r|
        create table if not exists unique_claims
            ( value  text
            , claims set<uuid>
            , primary key (value)
            ) with compaction = {'class': 'LeveledCompactionStrategy'}
              and gc_grace_seconds = 0;
    |]
  schema'
    [r|
        create table if not exists user_handle
            ( handle text
            , user   uuid
            , primary key (handle)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        alter table user add handle text;
    |]
