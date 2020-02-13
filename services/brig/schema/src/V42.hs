module V42 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 42 "Remove user.tracking_id"
    $ void
    $ schema'
      [r|
       alter columnfamily user drop tracking_id;
       |]
