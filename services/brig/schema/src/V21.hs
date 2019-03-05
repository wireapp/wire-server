module V21 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 21 "Remove password_reset.email" $ do
    void $ schema' [r|
       alter columnfamily password_reset drop email;
       |]
