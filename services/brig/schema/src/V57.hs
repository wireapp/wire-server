{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V57 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 57 "Add managed_by" $ do
    schema' [r| alter table user add managed_by int; |]
