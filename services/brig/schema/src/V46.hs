module V46 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 46 "Add summary field to service table" $
    schema'
      [r|
        alter table service add summary text
    |]
