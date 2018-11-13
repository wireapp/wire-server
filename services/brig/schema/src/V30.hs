{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V30 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 30 "Add even more client properties" $ do
    schema' [r| alter columnfamily clients add ip inet; |]
    schema' [r| alter columnfamily clients add lat double; |]
    schema' [r| alter columnfamily clients add lon double; |]
