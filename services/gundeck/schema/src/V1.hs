module V1 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 1 "Initial schema" $ do
  schema'
    [r|
        create columnfamily if not exists push
            ( ptoken    text -- token
            , app       text -- application
            , transport int  -- transport type
            , usr       uuid -- user id
            , primary key (ptoken, app, transport)
            ) with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  schema'
    [r|
        create columnfamily if not exists user_push
            ( usr       uuid -- user id
            , ptoken    text -- token
            , app       text -- application
            , transport int  -- transport type
            , primary key (usr, ptoken, app, transport)
            ) with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
