{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V32 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 32 "Add generic verification codes" $
    schema' [r|
        create columnfamily if not exists codes
            ( user    uuid
            , scope   int
            , code    text
            , retries int
            , primary key (user, scope)
            );
    |]
