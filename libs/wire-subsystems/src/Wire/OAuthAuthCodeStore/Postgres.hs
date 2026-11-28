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

module Wire.OAuthAuthCodeStore.Postgres
  ( interpretOAuthAuthCodeStoreToPostgres,
  )
where

import Data.Id (OAuthClientId, UserId)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.OAuth
import Wire.API.PostgresMarshall
import Wire.OAuthAuthCodeStore
import Wire.Postgres

-- | Rows carry an @expires_at@ deadline that every read filters on, so expired
-- codes are unusable even before they are physically removed. Expired rows are
-- purged by the @expire-oauth-auth-code@ pg_cron job (see migration
-- @20261127110318-oauth-auth-code-ttl-cleanup.sql@).
interpretOAuthAuthCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor OAuthAuthCodeStore r
interpretOAuthAuthCodeStoreToPostgres = interpret $ \case
  InsertOAuthAuthCode ttl code cid uid scope uri chal ->
    runStatement (code, cid, uid, scope, uri, chal, ttl) insertOAuthAuthCodeStatement
  LookupAndDeleteOAuthAuthCode code ->
    runStatement code lookupAndDeleteOAuthAuthCodeStatement

insertOAuthAuthCodeStatement ::
  Hasql.Statement
    (OAuthAuthorizationCode, OAuthClientId, UserId, OAuthScopes, RedirectUrl, OAuthCodeChallenge, Int32)
    ()
insertOAuthAuthCodeStatement =
  -- @expires_at@ is computed from the database clock (the same clock the
  -- lookup compares against) from the TTL in seconds. Codes are random, so a
  -- conflict is not expected; @DO NOTHING@ keeps an existing row untouched.
  lmapPG insertDb
  where
    insertDb :: Hasql.Statement (Text, UUID, UUID, Vector Text, Text, Text, Int32) ()
    insertDb =
      [resultlessStatement|
      INSERT INTO oauth_auth_code (code, client, user_id, scope, redirect_uri, code_challenge, expires_at)
      VALUES ($1 :: text, $2 :: uuid, $3 :: uuid, $4 :: text[], $5 :: text, $6 :: text, now() + make_interval(secs => $7 :: int4))
      ON CONFLICT (code) DO NOTHING
    |]

-- | Consume-on-read in a single atomic statement: the row is deleted whether or
-- not it has expired (codes are single-use either way), but it is only
-- returned while still live, so a code is returned at most once and expired
-- codes are indistinguishable from unknown ones.
lookupAndDeleteOAuthAuthCodeStatement ::
  Hasql.Statement
    OAuthAuthorizationCode
    (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
lookupAndDeleteOAuthAuthCodeStatement =
  dimapPG ((>>= onlyLive) <$> lookupAndDeleteDb)
  where
    lookupAndDeleteDb :: Hasql.Statement Text (Maybe (UUID, UUID, Vector Text, Text, Maybe Text, Bool))
    lookupAndDeleteDb =
      [maybeStatement|
      DELETE FROM oauth_auth_code
      WHERE code = ($1 :: text)
      RETURNING (client :: uuid), (user_id :: uuid), (scope :: text[]), (redirect_uri :: text), (code_challenge :: text?), ((expires_at > now()) :: bool)
    |]
    onlyLive (cid, uid, scope, uri, chal, isLive) = (cid, uid, scope, uri, chal) <$ guard isLive
