{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V25 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

-- TODO: use gc_grace_seconds = 0 as done in https://github.com/wireapp/wire-server/blob/develop/services/brig/schema/src/V34.hs#L26 ?
migration :: Migration
migration = Migration 25 "Add conversation_codes table" $
    schema' [r|
        CREATE TABLE conversation_codes (
            key           ascii, -- opaque conversation ID
            conversation  uuid,
            value         ascii, -- secret value
            PRIMARY KEY (key, conversation)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
