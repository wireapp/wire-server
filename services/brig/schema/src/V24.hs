{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V24 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 24 "Add user.language and user.country" $ do
    void $ schema' [r|
       alter columnfamily user add language ascii;
       |]
    void $ schema' [r|
       alter columnfamily user add country ascii;
       |]
