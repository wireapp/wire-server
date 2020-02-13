module V31 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 31 "Add model to clients" $
    schema' [r| alter columnfamily clients add model text; |]
