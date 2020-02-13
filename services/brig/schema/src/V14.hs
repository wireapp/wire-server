module V14 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 14 "Increase gc_grace_seconds back to 10 days" $ do
  void $
    schema'
      [r|
        alter columnfamily user with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily user_keys with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily activation_keys with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily password_reset with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily push with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily connection with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily invitation with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily invitee_info with gc_grace_seconds = 864000;
        |]
