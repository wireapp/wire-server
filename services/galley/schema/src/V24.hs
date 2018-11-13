{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V24 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 24 "Use LeveledCompactionStrategy on clients" $
    schema' [r|
        ALTER TABLE clients
            WITH compaction = {'class': 'LeveledCompactionStrategy'};
        |]
