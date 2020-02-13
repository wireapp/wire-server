module V35 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 35 "Add service provider tables" $ do
  schema'
    [r|
        create table if not exists provider
            ( id       uuid
            , name     text
            , email    text
            , password blob
            , url      blob
            , descr    text
            , primary key (id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        create table if not exists provider_keys
            ( key      text
            , provider uuid
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        create type if not exists pubkey
            ( typ  int
            , size int
            , pem  blob
            );
    |]
  schema'
    [r|
        create table if not exists service
            ( provider     uuid
            , id           uuid
            , name         text
            , descr        text
            , base_url     blob
            , auth_tokens  list<ascii>
            , pubkeys      list<frozen<pubkey>>
            , fingerprints list<blob>
            , assets       list<frozen<asset>>
            , tags         set<bigint>
            , enabled      boolean
            , primary key (provider, id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        create table if not exists service_tag
            ( bucket   int
            , tag      bigint
            , name     text
            , service  uuid
            , provider uuid
            , primary key ((bucket, tag), name, service)
            ) with clustering order by (name asc, service asc)
              and compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        alter table user add provider uuid;
    |]
  schema'
    [r|
        alter table user add service uuid;
    |]
