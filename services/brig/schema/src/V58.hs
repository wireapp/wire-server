{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module V58
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 58 "Add table for storing rich info" $ do
  void $
    schema'
      [r|
        create table if not exists rich_info
            ( user uuid
            , json blob
            , primary key (user)
            )
    |]
