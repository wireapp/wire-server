{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V16 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 16 "Drop push column family" $ do
    void $ schema' [r|
       drop columnfamily if exists push;
       |]
