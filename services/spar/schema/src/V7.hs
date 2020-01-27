module V7 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 7 "Add extra idp flag for default SSO code" $ do

    void $ schema' [r|
        ALTER TABLE idp ADD is_default boolean;
    |]
