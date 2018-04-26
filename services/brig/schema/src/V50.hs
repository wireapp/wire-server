{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V50 (migration) where

import Cassandra.Schema
import Data.Functor (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 50 "Support journal-based verification" $ do
    void $ schema' [r|
        create columnfamily if not exists journal_entries
            ( journal   uuid        -- the entry comes from that journal
            , index     int         -- entry's index in the journal
            , data      blob        -- serialized entry data
            , primary key (journal, index)
            ) with clustering order by (index desc);
        |]

    void $ schema' [r|
        alter columnfamily user
            add journal uuid;       -- currently active journal for the user
                                    -- (may not necessarily be present)
    |]
