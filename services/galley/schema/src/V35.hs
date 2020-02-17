module V35 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 35 "Delete deprecated legalhold_team_config" $ do
  schema'
    [r|
        DROP TABLE  legalhold_team_config;
    |]
