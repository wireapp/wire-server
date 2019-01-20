{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V15 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 15 "Add user.tracking_id" $ do
    void $ schema' [r|
       alter columnfamily user add tracking_id uuid;
       |]
