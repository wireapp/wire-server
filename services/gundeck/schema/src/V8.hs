{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V8 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 7 "Add fallback_cancel table" $ do
    void $ schema' [r| drop column family if exists fallback_cancel; |];
    void $ schema' [r| alter columnfamily user_push drop fallback; |]
