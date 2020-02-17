module V6 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 6 "Add fallback_cancel table" $ do
  schema'
    [r|
        -- Column family for keeping track of cancelled
        -- fallback notifications.
        create columnfamily if not exists fallback_cancel
            ( user uuid     -- user id
            , id   timeuuid -- notification id
            , primary key (user, id)
            ) with compaction = { 'class' : 'LeveledCompactionStrategy' }
               and gc_grace_seconds = 0;
        |]
-- TODO: fallback is deprecated as of https://github.com/wireapp/wire-server/pull/531
