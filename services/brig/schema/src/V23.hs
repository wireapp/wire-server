module V23 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 23 "Add password_reset.retries and timeout" $ do
  void $
    schema'
      [r|
       alter columnfamily password_reset add retries int;
       |]
  void $
    schema'
      [r|
       alter columnfamily password_reset add timeout timestamp;
       |]
