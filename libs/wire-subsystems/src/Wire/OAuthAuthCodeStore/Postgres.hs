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
-- @20261020130000-oauth-auth-code-ttl-cleanup.sql@).
interpretOAuthAuthCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor OAuthAuthCodeStore r
interpretOAuthAuthCodeStoreToPostgres = interpret $ \case
  InsertOAuthAuthCode ttl code cid uid scope uri chal ->
    runStatement (code, cid, uid, scope, uri, chal, ttl) insertOAuthAuthCodeStatement
  LookupAndDeleteOAuthAuthCode code ->
    lookupAndDeleteOAuthAuthCodePg code

insertOAuthAuthCodeStatement ::
  Hasql.Statement
    (OAuthAuthorizationCode, OAuthClientId, UserId, OAuthScopes, RedirectUrl, OAuthCodeChallenge, Int32)
    ()
insertOAuthAuthCodeStatement =
  lmapPG insertOAuthAuthCodeStatementDb

insertOAuthAuthCodeStatementDb ::
  Hasql.Statement
    (Text, UUID, UUID, Vector Text, ByteString, Text, Int32)
    ()
insertOAuthAuthCodeStatementDb =
  -- @expires_at@ is computed from the database clock (the same clock the
  -- lookup compares against) from the TTL in seconds. Codes are random, so a
  -- conflict is not expected; @DO NOTHING@ keeps an existing row untouched.
  [resultlessStatement|
      INSERT INTO oauth_auth_code (code, client, user_id, scope, redirect_uri, code_challenge, expires_at)
      VALUES ($1 :: text, $2 :: uuid, $3 :: uuid, $4 :: text[], $5 :: bytea, $6 :: text, now() + make_interval(secs => $7 :: int4))
      ON CONFLICT (code) DO NOTHING
    |]

-- | Consume-on-read in a single atomic statement: a code is returned at most
-- once and expired rows are indistinguishable from unknown ones.
lookupAndDeleteOAuthAuthCodePg ::
  (PGConstraints r) =>
  OAuthAuthorizationCode ->
  Sem r (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
lookupAndDeleteOAuthAuthCodePg code =
  runStatement code lookupAndDeleteOAuthAuthCodeStatement

lookupAndDeleteOAuthAuthCodeStatement ::
  Hasql.Statement
    OAuthAuthorizationCode
    (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
lookupAndDeleteOAuthAuthCodeStatement =
  dimapPG lookupAndDeleteOAuthAuthCodeStatementDb

lookupAndDeleteOAuthAuthCodeStatementDb ::
  Hasql.Statement
    Text
    (Maybe (UUID, UUID, Vector Text, ByteString, Maybe Text))
lookupAndDeleteOAuthAuthCodeStatementDb =
  [maybeStatement|
      DELETE FROM oauth_auth_code
      WHERE code = ($1 :: text) AND expires_at > now()
      RETURNING (client :: uuid), (user_id :: uuid), (scope :: text[]), (redirect_uri :: bytea), (code_challenge :: text?)
    |]
