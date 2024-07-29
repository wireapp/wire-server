module Galley.Schema.V93_ConferenceCallingSftForOneToOne where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 93 "Add conference_calling_one_to_one and status to team_features" $
    -- the existing field `conference_calling` is now repurposed to represent the lock status
    schema'
      [r| ALTER TABLE team_features ADD (
            conference_calling_one_to_one int,
            conference_calling_status int
        )
     |]
