module V26 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 26 "Add conversation access_role" $
    schema' [r| ALTER TABLE conversation ADD access_role int; |]
