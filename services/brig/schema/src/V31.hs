
module V31 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 31 "Add model to clients" $
    schema' [r| alter columnfamily clients add model text; |]
