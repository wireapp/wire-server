module V59 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 59 "Add table for storing whitelisted email addresses" $ do
    void $ schema' [r|
        create columnfamily if not exists whitelist
            ( key   text -- email/phone that is whitelisted
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
