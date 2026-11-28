-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE TemplateHaskell #-}

-- | Store for OAuth PKCE authorization codes (the @oauth_auth_code@ table).
-- Codes are single-use and short-lived (a few hundred seconds): 'lookupAndDeleteOAuthAuthCode'
-- consumes the code on read.
module Wire.OAuthAuthCodeStore where

import Data.Id (OAuthClientId, UserId)
import Imports
import Polysemy
import Wire.API.OAuth

data OAuthAuthCodeStore m a where
  InsertOAuthAuthCode ::
    -- | TTL in seconds
    Int32 ->
    OAuthAuthorizationCode ->
    OAuthClientId ->
    UserId ->
    OAuthScopes ->
    RedirectUrl ->
    OAuthCodeChallenge ->
    OAuthAuthCodeStore m ()
  -- | Consume-on-read: the code is deleted as part of the lookup, and expired
  -- codes are indistinguishable from unknown ones.
  --
  -- Atomicity depends on the backend: the Postgres interpreter returns a code
  -- at most once (single @DELETE … RETURNING@), whereas the Cassandra
  -- interpreter reads then deletes in two steps, so concurrent lookups of the
  -- same code may both succeed.
  LookupAndDeleteOAuthAuthCode ::
    OAuthAuthorizationCode ->
    OAuthAuthCodeStore
      m
      (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))

makeSem ''OAuthAuthCodeStore
