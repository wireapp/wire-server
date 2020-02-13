module V32 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 32 "Migrate User Legal Hold Status to Team Members Table" $ do
  schema'
    [r|
        ALTER TABLE team_member
          ADD
        (
            legalhold_status             int
        )
    |]
  schema'
    [r|
        DROP TABLE legalhold_user_status
    |]
