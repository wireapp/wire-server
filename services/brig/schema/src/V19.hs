{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V19 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 19 "Add properties" $ do
    void $ schema' [r|
        create columnfamily if not exists properties
            ( user   uuid
            , key    ascii
            , value  blob
            , primary key (user, key)
            );
       |]
