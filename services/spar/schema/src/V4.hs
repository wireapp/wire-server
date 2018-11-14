{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V4 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 4 "Store SCIM authentication tokens" $ do
    -- Tables containing tokens used for authenticating user provisioning
    -- tools (e.g. Okta).
    --
    -- Notes:
    --
    -- 1. We can have several tokens per team (if the team uses several
    --    provisioning tools), and it should be possible to revoke one
    --    without revoking the other.
    --
    -- 2. Each token can have an IdP associated with it; this will be the
    --    IdP used to authenticate the user.
    --
    -- 3. We need to be able to query the team by the token (when doing
    --    authentication); however, we also need to be able to list all
    --    tokens belonging to a team (when displaying tokens on the team
    --    settings page). This means that we have to maintain two tables.
    void $ schema' [r|
        CREATE TABLE if not exists team_provisioning
            ( team          uuid
            , token         text
            , idp           uuid         -- optional
            , PRIMARY KEY (team, token)
            );

        CREATE TABLE if not exists team_provisioning_rev
            ( team          uuid
            , token         text
            , idp           uuid         -- optional
            , PRIMARY KEY (token, team)
            );
   |]
