{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V17 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 17 "Drop user_push column family" $ do
    void $ schema' [r|
       drop columnfamily if exists user_push;
       |]
