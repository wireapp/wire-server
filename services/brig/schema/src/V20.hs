module V20 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 20 "Add activation_keys.challenge" $ do
  void $
    schema'
      [r|
       alter columnfamily activation_keys add challenge ascii;
       |]
