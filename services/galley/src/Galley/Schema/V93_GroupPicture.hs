module Galley.Schema.V93_GroupPicture where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 93 "Add group picture as a combination of background colour and an emoji to group conversations" $
    schema'
      [r| ALTER TABLE conversation ADD (
            background_colour text,
            emoji text
        )
      |]
