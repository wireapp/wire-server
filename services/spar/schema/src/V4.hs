module V4 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 4 "Store SCIM authentication tokens" $ do
    -- docs/developer/scim/storage.md {#DevScimStorageTokens}

    -- Tables containing tokens used for authenticating user provisioning
    -- tools (e.g. Okta).
    --
    -- Notes:
    --
    -- 1. We can have several tokens per team (if the team uses several
    --    provisioning tools), and it should be possible to revoke one
    --    without revoking the other. However, for each token there can only
    --    be one team.
    --
    -- 2. Each token can have an IdP associated with it; this will be the
    --    IdP used to authenticate the user.
    void $ schema' [r|
        CREATE TABLE if not exists team_provisioning_by_token
            ( token_        text
            , team          uuid
            , id            uuid
            , created_at    timestamp
            , idp           uuid         -- optional
            , descr         text
            , PRIMARY KEY (token_)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]

    -- We also need to be able to list all tokens belonging to a team (when
    -- displaying tokens on the team settings page).
    void $ schema' [r|
        CREATE TABLE if not exists team_provisioning_by_team
            ( token_        text
            , team          uuid
            , id            uuid
            , created_at    timestamp
            , idp           uuid         -- optional
            , descr         text
            , PRIMARY KEY (team, id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
