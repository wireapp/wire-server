module V7 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 7 "Store default SSO code" $ do

    -- primary_key_always_default should always be "default".
    -- It exists to guarantee that there is only single default idp at all times.
    void $ schema' [r|
        CREATE TABLE if not exists default_idp
            ( primary_key_always_default text
            , idp uuid
            , PRIMARY KEY (primary_key_always_default)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
