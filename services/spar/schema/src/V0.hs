{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V0 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

-- TODO: not sure about some of the primary keys, but the ones i picked at least need to be indices.
-- TODO: use `USING TTL` (grep it in services/brig) for garbage collection in authreq, authresp tables.

migration :: Migration
migration = Migration 0 "Initial schema" $ do
    void $ schema' [r|
        create columnfamily if not exists authreq
            ( req          text
            , end_of_life  timestamp
            , primary key  req
            );
        |]

    void $ schema' [r|
        create columnfamily if not exists authresp
            ( resp         text
            , end_of_life  timestamp
            , primary key  req
            );
        |]

    {- this will be needed later, but for now we will get all the information in here from the config.
    void $ schema' [r|
        create columnfamily if not exists idp
            ( idp      uuid
            , idp_name text
            , owners    list<uuid>
            -- ...
            , primary key (idp_name)
            );
        |]
    -}
