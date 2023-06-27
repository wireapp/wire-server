{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module V78_ConnectionRemoteIndex
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 78 "Add a secondary index for federated (remote) connections" $ do
  schema'
    [r| CREATE INDEX on connection_remote (right_domain)
      |]
