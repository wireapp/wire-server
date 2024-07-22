module Galley.Schema.V93_ConferenceCallingSftForOneToOne where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 93 "Add conference_calling_sft_for_one_to_one and its lock_status to team_features" $
    schema'
      [r| ALTER TABLE team_features ADD (
            conference_calling_sft_for_one_to_one boolean,
            conference_calling_lock_status int
        )
     |]
