module V55 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 55 "Add optional role to team invitations" $ do
  schema' [r| alter table team_invitation add role int; |]
