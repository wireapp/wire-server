{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V20 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 20 "Add activation_keys.challenge" $ do
    void $ schema' [r|
       alter columnfamily activation_keys add challenge ascii;
       |]
