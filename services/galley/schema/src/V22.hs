{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V22 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 22 "Added bound flag to teams" $ do
    schema' [r| ALTER TABLE team ADD bound boolean; |]
