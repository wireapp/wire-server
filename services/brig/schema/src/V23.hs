{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V23 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 23 "Add password_reset.retries and timeout" $ do
    void $ schema' [r|
       alter columnfamily password_reset add retries int;
       |]

    void $ schema' [r|
       alter columnfamily password_reset add timeout timestamp;
       |]
