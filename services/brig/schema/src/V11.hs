
module V11 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 11 "Add user.status column" $ do
    void $ schema' [r|
        alter columnfamily user add status int;
        |]
