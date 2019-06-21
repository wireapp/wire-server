
module V33 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 33 "xxx" $ do
    schema' [r|
        create type if not exists pubkey
            ( typ  int
            , size int
            , pem  blob
            );
    |]
    schema' [r|
        ALTER TABLE legalhold_service
          ADD
        (
            pubkey             pubkey
        )
    |]