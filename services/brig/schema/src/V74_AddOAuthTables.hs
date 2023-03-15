{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module V74_AddOAuthTables
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 74 "Add table for OAuth clients" $ do
    schema'
      [r|
        CREATE TABLE IF NOT EXISTS oauth_client
          ( id uuid PRIMARY KEY
          , name text
          , redirect_uri blob
          , secret blob
          )
     |]
    schema'
      [r|
        CREATE TABLE IF NOT EXISTS oauth_auth_code
          ( code ascii PRIMARY KEY
          , client uuid
          , user uuid
          , scope set<text>
          , redirect_uri blob
          ) WITH default_time_to_live = 300;
     |]
    schema'
      [r|
        CREATE TABLE IF NOT EXISTS oauth_refresh_token
          ( id uuid PRIMARY KEY
          , client uuid
          , user uuid
          , scope set<text>
          , created_at timestamp
          ) WITH default_time_to_live = 14515200; -- 24 weeks
     |]
    schema'
      [r|
        CREATE TABLE IF NOT EXISTS oauth_user_refresh_token
          ( user uuid
          , token_id uuid
          , PRIMARY KEY (user, token_id)
          ) WITH default_time_to_live = 14515200; -- 24 weeks
     |]
