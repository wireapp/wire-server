{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V17 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 17 "Drop user_push column family" $ do
    -- 'user_push' has been moved to gundeck
    void $ schema' [r|
       drop columnfamily if exists user_push;
       |]
