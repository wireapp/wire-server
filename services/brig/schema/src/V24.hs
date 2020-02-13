module V24 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 24 "Add user.language and user.country" $ do
  void $
    schema'
      [r|
       alter columnfamily user add language ascii;
       |]
  void $
    schema'
      [r|
       alter columnfamily user add country ascii;
       |]
