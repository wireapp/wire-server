{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V59 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 59 "Store extra headers for services" $ do
    void $ schema' [r|
        create type if not exists service_headers
            ( name text
            , value text
            );
    |]
    void $ schema' [r|
        alter table service add extra_headers set<frozen<service_headers>>;
    |]
