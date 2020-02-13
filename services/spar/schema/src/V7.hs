module V7 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 7 "Store default SSO code" $ do
  -- partition_key_always_default should always be "default".
  -- It exists so the row is always at a known partition.
  void $
    schema'
      [r|
        CREATE TABLE if not exists default_idp
            ( partition_key_always_default text
            , idp uuid
            , PRIMARY KEY (partition_key_always_default, idp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
