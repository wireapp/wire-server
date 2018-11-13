{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V28 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 28 "Add additional client properties" $ do
    schema' [r| alter columnfamily clients add class int; |]
    schema' [r| alter columnfamily clients add cookie text; |]
