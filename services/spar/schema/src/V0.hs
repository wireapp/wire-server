{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V0 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 0 "Initial schema" $ do
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
