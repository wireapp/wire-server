{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V22 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 22 "Add login_codes" $ do
    void $ schema' [r|
        create columnfamily if not exists login_codes
            ( user    uuid
            , code    text
            , retries int
            , timeout timestamp
            , primary key (user)
            );
       |]
