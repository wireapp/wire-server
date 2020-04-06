-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module V4 (migration) where

import Cassandra.Schema
import Imports
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
  void $
    schema'
      [r|
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
  void $
    schema'
      [r|
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
