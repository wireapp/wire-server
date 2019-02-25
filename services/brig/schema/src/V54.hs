module V54 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 54 "Add metadata to team invitations" $ do
    schema' [r| alter table team_invitation add created_by uuid; |]
