module V11 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 11 "Add user.status column" $ do
  void $
    schema'
      [r|
        alter columnfamily user add status int;
        |]
