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

-- | Postgres storage for verification codes and their throttle state.
--
-- Cassandra stores these rows with a TTL and derives the remaining lifetime via
-- @ttl(...)@ on read. Here rows carry an @expires_at@ column; the remaining
-- lifetime is computed as @expires_at - now()@, and the
-- @expires_at > now()@ guard reproduces Cassandra's vanished-row semantics.
module Wire.VerificationCodeStore.Postgres
  ( interpretVerificationCodeStoreToPostgres,
    insertCodeRow,
    insertThrottleRow,
  )
where

import Data.RetryAfter (RetryAfter (..))
import Data.UUID (UUID)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User.Identity (EmailAddress)
import Wire.Postgres
import Wire.VerificationCode
import Wire.VerificationCodeStore (VerificationCodeStore (..))

interpretVerificationCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor VerificationCodeStore r
interpretVerificationCodeStoreToPostgres = interpret $ \case
  InsertCode c ->
    insertCodeRow
      ( c.codeKey,
        c.codeScope,
        c.codeValue,
        c.codeRetries,
        c.codeFor,
        c.codeAccount,
        round (codeTTL c)
      )
  LookupCode key scope -> lookupCode key scope
  DeleteCode key scope -> deleteCode key scope
  -- Cassandra binds the initial delay as both the stored column and the row
  -- TTL; mirror that here.
  InsertThrottle key scope t ->
    insertThrottleRow (key, scope, fromIntegral t, fromIntegral t)
  LookupThrottle key scope -> lookupThrottle key scope

-- | Upsert one @vcodes@ row. Mirrors Cassandra's @INSERT ... USING TTL@, which
-- is an upsert that resets the TTL, hence the @DO UPDATE@ clause.
insertCodeRow ::
  (PGConstraints r) =>
  (Key, Scope, Value, Retries, EmailAddress, Maybe UUID, Int32) ->
  Sem r ()
insertCodeRow row = runStatement row insertCodeRowStatement

insertCodeRowStatement ::
  Hasql.Statement (Key, Scope, Value, Retries, EmailAddress, Maybe UUID, Int32) ()
insertCodeRowStatement =
  lmapPG
    [resultlessStatement|INSERT INTO vcodes (key, scope, value, retries, email, account, expires_at)
                         VALUES
                           ($1 :: text, $2 :: int4, $3 :: text, $4 :: int4, $5 :: text, $6 :: uuid?, now() + make_interval(secs => $7 :: int4))
                         ON CONFLICT (key, scope) DO UPDATE
                         SET value = ($3 :: text),
                             retries = ($4 :: int4),
                             email = ($5 :: text),
                             account = ($6 :: uuid?),
                             expires_at = now() + make_interval(secs => $7 :: int4)
       |]

lookupCode ::
  (PGConstraints r) =>
  Key ->
  Scope ->
  Sem r (Maybe Code)
lookupCode key scope = do
  mRow <- runStatement (key, scope) selectCodeRowStatement
  pure $ fmap toCode mRow
  where
    toCode ::
      (Value, Retries, EmailAddress, Maybe UUID, Int32) ->
      Code
    toCode (v, r, e, a, remaining) =
      Code
        { codeKey = key,
          codeScope = scope,
          codeValue = v,
          codeRetries = r,
          codeTTL = Timeout (fromIntegral remaining),
          codeFor = e,
          codeAccount = a
        }

selectCodeRowStatement ::
  Hasql.Statement (Key, Scope) (Maybe (Value, Retries, EmailAddress, Maybe UUID, Int32))
selectCodeRowStatement =
  dimapPG
    [maybeStatement|SELECT (value :: text), (retries :: int4), (email :: text), (account :: uuid?), (floor(extract(epoch from (expires_at - now()))) :: int4)
                    FROM vcodes
                    WHERE key = ($1 :: text) AND scope = ($2 :: int4) AND expires_at > now()
                   |]

deleteCode ::
  (PGConstraints r) =>
  Key ->
  Scope ->
  Sem r ()
deleteCode key scope = runStatement (key, scope) deleteCodeStatement

deleteCodeStatement :: Hasql.Statement (Key, Scope) ()
deleteCodeStatement =
  lmapPG
    [resultlessStatement|DELETE FROM vcodes
                         WHERE key = ($1 :: text) AND scope = ($2 :: int4)
       |]

-- | Upsert one @vcodes_throttle@ row. @initialDelay@ and @ttlSecs@ are separate
-- parameters because the migration copy loop carries the original delay along
-- with the row's remaining TTL.
insertThrottleRow ::
  (PGConstraints r) =>
  (Key, Scope, Int32, Int32) ->
  Sem r ()
insertThrottleRow row = runStatement row insertThrottleRowStatement

insertThrottleRowStatement :: Hasql.Statement (Key, Scope, Int32, Int32) ()
insertThrottleRowStatement =
  lmapPG
    [resultlessStatement|INSERT INTO vcodes_throttle (key, scope, initial_delay, expires_at)
                         VALUES
                           ($1 :: text, $2 :: int4, $3 :: int4, now() + make_interval(secs => $4 :: int4))
                         ON CONFLICT (key, scope) DO UPDATE
                         SET initial_delay = ($3 :: int4),
                             expires_at = now() + make_interval(secs => $4 :: int4)
       |]

lookupThrottle ::
  (PGConstraints r) =>
  Key ->
  Scope ->
  Sem r (Maybe RetryAfter)
lookupThrottle key scope = do
  mRemaining <- runStatement (key, scope) selectThrottleRowStatement
  pure $ RetryAfter . fromIntegral <$> mRemaining

selectThrottleRowStatement :: Hasql.Statement (Key, Scope) (Maybe Int32)
selectThrottleRowStatement =
  dimapPG
    [maybeStatement|SELECT (floor(extract(epoch from (expires_at - now()))) :: int4)
                    FROM vcodes_throttle
                    WHERE key = ($1 :: text) AND scope = ($2 :: int4) AND expires_at > now()
                   |]
