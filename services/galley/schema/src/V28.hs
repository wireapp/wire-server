module V28 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 28 "Add (extra) otr muted status to member" $
    schema' [r| ALTER TABLE member ADD otr_muted_status int; |]
