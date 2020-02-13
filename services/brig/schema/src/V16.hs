module V16 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 16 "Drop push column family" $ do
  void $
    schema'
      [r|
       drop columnfamily if exists push;
       |]
