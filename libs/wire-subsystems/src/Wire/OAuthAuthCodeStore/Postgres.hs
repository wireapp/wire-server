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
    insertOAuthAuthCodePg,
  )
where

import Data.Id (OAuthClientId, UserId)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
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

-- TTL/purge note: auth code rows carry @expires_at@ and are filtered by read
-- predicates; expired rows are not physically purged.  No purge job is
-- provisioned: expired codes are unusable (unreachable on every read path)
-- and bounded by code issuance volume, following the accepted no-purge
-- precedent of the password_reset migration.
interpretOAuthAuthCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor OAuthAuthCodeStore r
interpretOAuthAuthCodeStoreToPostgres = interpret $ \case
  InsertOAuthAuthCode ttl code cid uid scope uri chal -> do
    now <- liftIO getCurrentTime
    insertOAuthAuthCodePg (addUTCTime (fromIntegral ttl) now) code cid uid scope uri (Just chal)
  LookupAndDeleteOAuthAuthCode code ->
    lookupAndDeleteOAuthAuthCodePg code

-- | Exposed separately (with a 'Maybe' challenge) for the migration loop,
-- which copies rows carrying a NULL @code_challenge@. The first argument is
-- the absolute @expires_at@ deadline, computed by the caller.
insertOAuthAuthCodePg ::
  (PGConstraints r) =>
  UTCTime ->
  OAuthAuthorizationCode ->
  OAuthClientId ->
  UserId ->
  OAuthScopes ->
  RedirectUrl ->
  Maybe OAuthCodeChallenge ->
  Sem r ()
insertOAuthAuthCodePg expiresAt code cid uid scope uri mChal =
  runStatement
    (code, cid, uid, scope, uri, mChal, expiresAt)
    insertOAuthAuthCodeStatement

insertOAuthAuthCodeStatement ::
  Hasql.Statement
    (OAuthAuthorizationCode, OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge, UTCTime)
    ()
insertOAuthAuthCodeStatement =
  lmapPG insertOAuthAuthCodeStatementDb

insertOAuthAuthCodeStatementDb ::
  Hasql.Statement
    (Text, UUID, UUID, Vector Text, ByteString, Maybe Text, UTCTime)
    ()
insertOAuthAuthCodeStatementDb =
  -- NOTE: expiry is computed by the caller from its own clock (brig from its
  -- configured expiration, the migration from the source row's TTL); brig
  -- always configures a positive expiration. @DO NOTHING@ leaves an existing
  -- row (e.g. already dual-written) untouched — no expiry refresh.
  [resultlessStatement|
      INSERT INTO oauth_auth_code (code, client, "user", scope, redirect_uri, code_challenge, expires_at)
      VALUES ($1 :: text, $2 :: uuid, $3 :: uuid, $4 :: text[], $5 :: bytea, $6 :: text?, $7 :: timestamptz)
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
      RETURNING (client :: uuid), ("user" :: uuid), (scope :: text[]), (redirect_uri :: bytea), (code_challenge :: text?)
    |]
