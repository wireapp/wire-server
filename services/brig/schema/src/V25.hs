module V25 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 25 "Change client IDs from ascii to text" $ do
  schema' [r| alter columnfamily clients alter client type text; |]
  schema' [r| alter columnfamily prekeys alter client type text; |]
