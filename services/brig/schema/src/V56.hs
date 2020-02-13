{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module V56
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 56 "Add table to exclude phone number prefixes" $ do
  -- A table for manual excluding of phone number prefixes that abuse sms sending
  -- and/or calling.
  --
  -- Operations we need to support:
  --   * Add a new prefix of arbitrary length
  --   * Remove a prefix
  --   * Given a phone number, check whether it matches any existing prefix
  void $
    schema'
      [r|
        create table if not exists excluded_phones
            ( prefix text
            , comment text
            , primary key (prefix)
            )
    |]
