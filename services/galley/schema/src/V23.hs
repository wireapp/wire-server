module V23 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 23 "Add status flag to team" $ do
  schema' [r| ALTER TABLE team ADD status int; |]
