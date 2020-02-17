module V48 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 48 "Add expiration to user table" $
    schema' [r| ALTER TABLE user ADD expires timestamp; |]
