{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V1 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 1 "Add verdict table" $ do

    -- TODO: currently 'format' is json encoded VerdictFormat. Should we store type and success/error URIs separately instead?
    void $ schema' [r|
        CREATE TABLE if not exists verdict
            ( req                   text
            , format_con            int
            , format_mobile_success text
            , format_mobile_error   text
            , primary key (req)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
