
module V2 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 2 "Add extra idp keys set" $ do

    void $ schema' [r|
        ALTER TABLE idp DROP metadata;
    |]

    void $ schema' [r|
        ALTER TABLE idp ADD extra_public_keys list<blob>;
    |]
