{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V4 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 4 "Store SCIM authentication tokens" $ do

    -- Notes:
    --
    -- 1. We can have several tokens per team (if the team uses several
    --    provisioning tools), and it should be possible to revoke one
    --    without revoking the other.
    --
    -- 2. Each token can have an IdP associated with it; this will be the
    --    IdP used to authenticate the user.
    void $ schema' [r|
        CREATE TABLE if not exists team_provisioning
            ( team          uuid
            , token         text
            , idp           uuid         -- optional
            , PRIMARY KEY (team, token)
            )
   |]
