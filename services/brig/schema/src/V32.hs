module V32 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 32 "Add generic verification codes" $
    schema'
      [r|
        create columnfamily if not exists codes
            ( user    uuid
            , scope   int
            , code    text
            , retries int
            , primary key (user, scope)
            );
    |]
