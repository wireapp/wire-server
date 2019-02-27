module V41 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 41 "Add searchable field to user table" $
    schema' [r|
        alter table user add searchable boolean
    |]
