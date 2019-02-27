module V50 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 50 "Add UserSSOId to user table" $
    schema' [r| ALTER TABLE user ADD sso_id text; |]
