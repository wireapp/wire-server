module V7 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 7 "Store default SSO code" $ do

    void $ schema' [r|
        CREATE TABLE if not exists default_idp
            ( idp uuid
            , PRIMARY KEY (idp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
