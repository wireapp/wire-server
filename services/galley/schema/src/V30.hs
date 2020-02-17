module V30 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 30 "Add invitation metadata to team_member" $ do
  schema' [r| ALTER TABLE team_member ADD invited_by uuid; |]
  schema' [r| ALTER TABLE team_member ADD invited_at timestamp; |]
