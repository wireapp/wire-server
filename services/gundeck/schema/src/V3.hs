module V3 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 3 "Add clients table, push.client and user_push.client" $ do
  schema'
    [r|
        create columnfamily if not exists clients
            ( user    uuid -- user id
            , client  text -- client id
            , enckey  blob -- native push encryption key
            , mackey  blob -- native push mac key
            , primary key (user, client)
            ) with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  -- TODO: REFACTOR: table clients is not used any more and should be removed at some point (as of
  -- removing the --prefer-notice flag for gundeck; Mon Dec 10 15:04:21 CET 2018)
  schema' [r| alter columnfamily user_push add client text; |]
  schema' [r| alter columnfamily push add client text; |]
