{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V48 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 48 "Add expiration to user table" $
    schema' [r| ALTER TABLE user ADD expires timestamp; |]
