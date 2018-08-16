{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V53 (migration) where

import Data.Functor (void)
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 53 "Add a table for tracking users spawned by services" $ do
    void $ schema' [r|
        create table if not exists service_user
            ( provider uuid
            , service  uuid
            , user     uuid
            , primary key (provider, service)
            )
    |]
