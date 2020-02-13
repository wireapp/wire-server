module V50 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 50 "Add UserSSOId to user table" $
    schema' [r| ALTER TABLE user ADD sso_id text; |]
