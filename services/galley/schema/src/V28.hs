
module V28 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 28 "Add (extra) otr muted status to member" $
    schema' [r| ALTER TABLE member ADD otr_muted_status int; |]
