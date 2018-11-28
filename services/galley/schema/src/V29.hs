{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V29 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 29 "Add conversation receipt mode" $
    schema' [r| ALTER TABLE conversation ADD receipt_mode int; |]
