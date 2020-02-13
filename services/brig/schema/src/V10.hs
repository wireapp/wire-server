module V10 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 10 "Switch to leveled compaction" $ do
  void $
    schema'
      [r|
        alter columnfamily user with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily user_keys with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily activation_keys with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily password_reset with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily push with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily connection with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily invitation with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily invitee_info with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
