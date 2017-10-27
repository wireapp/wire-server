{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V18 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 18 "Add prekeys" $ do
    void $ schema' [r|
        create columnfamily if not exists clients
            ( user   uuid
            , client text
            , tstamp timestamp
            , type   int
            , label  text
            , primary key (user, client)
            );
       |]

    void $ schema' [r|
        create columnfamily if not exists prekeys
            ( user   uuid
            , client text
            , key    int
            , data   text
            , primary key (user, client, key)
            );
       |]
