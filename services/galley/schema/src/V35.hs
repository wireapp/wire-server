module V35 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 35 "Delete deprecated legalhold_team_config" $ do
    schema' [r|
        DROP TABLE  legalhold_team_config;
    |]
