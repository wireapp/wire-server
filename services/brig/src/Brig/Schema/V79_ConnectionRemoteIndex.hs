{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Brig.Schema.V79_ConnectionRemoteIndex
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 79 "Add a secondary index for federated (remote) connections" $ do
  schema'
    [r| CREATE INDEX on connection_remote (right_domain)
      |]
