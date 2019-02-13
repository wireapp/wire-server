
module V55 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 55 "Add optional role to team invitations" $ do
    schema' [r| alter table team_invitation add role int; |]
