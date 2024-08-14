module Galley.Schema.V94_DummyConfig where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 94 "Add dummy feature" $
    schema'
      [r| ALTER TABLE team_features ADD (
            dummy_lock_status int,
            dummy_status int,
            dummy_level int
        )
     |]
