{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V25 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 25 "Change client IDs from ascii to text" $ do
    schema' [r| alter columnfamily clients alter client type text; |]
    schema' [r| alter columnfamily prekeys alter client type text; |]
