module V29 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 29 "Add conversation receipt mode" $
    schema' [r| ALTER TABLE conversation ADD receipt_mode int; |]
