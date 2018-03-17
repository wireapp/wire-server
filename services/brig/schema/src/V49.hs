{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V49 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 49 "Add binding team to user table" $
    schema' [r| ALTER TABLE user ADD team uuid; |]
