module V22 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 22 "Added binding flag to teams" $ do
    schema' [r| ALTER TABLE team ADD binding boolean; |]
