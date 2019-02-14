
module V30 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 30 "Add invitation metadata to team_member" $ do
    schema' [r| ALTER TABLE team_member ADD invited_by uuid; |]
    schema' [r| ALTER TABLE team_member ADD invited_at timestamp; |]
