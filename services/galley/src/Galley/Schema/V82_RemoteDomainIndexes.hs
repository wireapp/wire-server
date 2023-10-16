{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Galley.Schema.V82_RemoteDomainIndexes
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 82 "Add a secondary index for remote domains on local conversations, and remote conversations with local membership" $ do
  schema'
    [r| CREATE INDEX on member_remote_user (user_remote_domain)
      |]
  schema'
    [r| CREATE INDEX on user_remote_conv (conv_remote_domain)
      |]
