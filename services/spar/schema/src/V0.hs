{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V0 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 0 "Initial schema" $ do

    -- TODO: review authreq/authresp/user tables; `text` likely non-unique and thus a problematic primary key.
    -- see existing schema for inspiration: https://github.com/wireapp/wire-server/blob/312f82344fa153dad76a03d7280b4424174c2fac/doc/db_schema.cql
    -- TODO: on read-heavy columns, use
    --     with compaction = {'class': 'LeveledCompactionStrategy'};
    -- see https://www.datastax.com/dev/blog/when-to-use-leveled-compaction
    void $ schema' [r|
        create columnfamily if not exists authreq
            ( req          text
            , end_of_life  timestamp
            , primary key  (req)
            );
        |]

    void $ schema' [r|
        create columnfamily if not exists authresp
            ( resp         text
            , end_of_life  timestamp
            , primary key  (resp)
            );
        |]

    void $ schema' [r|
        create columnfamily if not exists user
            ( idp      text
            , sso_id   text
            , uid      uuid
            , primary key (idp, sso_id)
            );
        |]

        -- TODO: add a uuid for each idp?
        -- , idp           uuid
        -- , PRIMARY KEY (idp, team, issuer)
    void $ schema' [r|
        CREATE TABLE if not exists idp
            ( team          uuid
            , issuer        blob
            , path          text
            , metadata      blob
            , uri           blob
            , key           blob
            , PRIMARY KEY (issuer)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
