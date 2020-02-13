module V33
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 33 "Add storage for pubkey for LH services" $ do
  schema'
    [r|
        create type if not exists pubkey
            ( typ  int
            , size int
            , pem  blob
            );
    |]
  schema'
    [r|
        ALTER TABLE legalhold_service
          ADD
        (
            pubkey             pubkey
        )
    |]
