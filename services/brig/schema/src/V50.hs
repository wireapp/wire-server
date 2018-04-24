{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V50 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 50 "Add UserSSOId to user table" $
    schema' [r| ALTER TABLE user ADD ssoid text; |]
