module V39 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 39 "Add user_cookies table" $
    schema'
      [r|
        create table if not exists user_cookies
            ( user    uuid
            , expires timestamp
            , id      bigint
            , label   text
            , type    int
            , created timestamp
            , succ_id bigint
            , primary key (user, expires, id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
