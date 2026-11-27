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
-- lifetime is computed as @expires_at - now()@ (rounded up, so a live row never
-- reports a remaining lifetime of zero), and the @expires_at > now()@ guard
-- reproduces Cassandra's vanished-row semantics. Expired rows are deleted by
-- the pg_cron jobs @expire-vcodes@ and @expire-vcodes-throttle@ scheduled by the
-- TTL-cleanup migration (ADR-0005).
module Wire.VerificationCodeStore.Postgres
  ( interpretVerificationCodeStoreToPostgres,
    lookupCodeRaw,
  )
where

import Control.Exception (ErrorCall (..), throwIO)
import Data.RetryAfter (RetryAfter (..))
import Data.UUID (UUID)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Session (statement)
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
  InsertCode c -> insertCode c
  LookupCode key scope -> lookupCode key scope
  DeleteCode key scope -> deleteCode key scope
  DecrementCodeRetries key scope -> decrementCodeRetries key scope
  InsertThrottle key scope t -> insertThrottle key scope t
  LookupThrottle key scope -> lookupThrottle key scope

-- | Mirrors Cassandra's @INSERT ... USING TTL@, which is an upsert that resets
-- the TTL, hence the @DO UPDATE@ clause.
insertCode :: (PGConstraints r) => Code -> Sem r ()
insertCode c =
  runStatement
    (c.codeKey, c.codeScope, c.codeValue, c.codeRetries, c.codeFor, c.codeAccount, round (codeTTL c))
    insertCodeStatement

insertCodeStatement :: Hasql.Statement (Key, Scope, Value, Retries, EmailAddress, Maybe UUID, Int32) ()
insertCodeStatement =
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
lookupCode key scope = toCode key scope <$$> runStatement (key, scope) selectCodeStatement

-- | Look up a live code directly through a pool, bypassing the
-- 'VerificationCodeStore' effect. Exposed for the brig integration suite,
-- mirroring the raw Cassandra lookup via
-- 'Wire.VerificationCodeStore.Cassandra.lookupCodeImpl'. 'Hasql.UsageError'
-- has no 'Exception' instance in this hasql version, so failures surface as an
-- 'ErrorCall'.
lookupCodeRaw :: HasqlPoolExt.Pool -> Key -> Scope -> IO (Maybe Code)
lookupCodeRaw pool key scope =
  useWithResetAndRetry pool (statement (key, scope) selectCodeStatement)
    >>= either (throwIO . ErrorCall . show) (pure . fmap (toCode key scope))

toCode :: Key -> Scope -> (Value, Retries, EmailAddress, Maybe UUID, Int32) -> Code
toCode key scope (v, r, e, a, remaining) =
  Code
    { codeKey = key,
      codeScope = scope,
      codeValue = v,
      codeRetries = r,
      codeTTL = Timeout (fromIntegral remaining),
      codeFor = e,
      codeAccount = a
    }

selectCodeStatement ::
  Hasql.Statement (Key, Scope) (Maybe (Value, Retries, EmailAddress, Maybe UUID, Int32))
selectCodeStatement =
  dimapPG
    [maybeStatement|SELECT (value :: text), (retries :: int4), (email :: text), (account :: uuid?), (ceil(extract(epoch from (expires_at - now()))) :: int4)
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

-- | Atomically consume one retry of a live code, leaving @expires_at@
-- untouched.
decrementCodeRetries ::
  (PGConstraints r) =>
  Key ->
  Scope ->
  Sem r ()
decrementCodeRetries key scope = runStatement (key, scope) decrementCodeRetriesStatement

decrementCodeRetriesStatement :: Hasql.Statement (Key, Scope) ()
decrementCodeRetriesStatement =
  lmapPG
    [resultlessStatement|UPDATE vcodes
                         SET retries = retries - 1
                         WHERE key = ($1 :: text) AND scope = ($2 :: int4) AND retries > 0 AND expires_at > now()
       |]

-- | Cassandra binds the initial delay as both the stored column and the row
-- TTL; mirror that here.
insertThrottle ::
  (PGConstraints r) =>
  Key ->
  Scope ->
  Word ->
  Sem r ()
insertThrottle key scope t = runStatement (key, scope, fromIntegral t) insertThrottleStatement

insertThrottleStatement :: Hasql.Statement (Key, Scope, Int32) ()
insertThrottleStatement =
  lmapPG
    [resultlessStatement|INSERT INTO vcodes_throttle (key, scope, initial_delay, expires_at)
                         VALUES
                           ($1 :: text, $2 :: int4, $3 :: int4, now() + make_interval(secs => $3 :: int4))
                         ON CONFLICT (key, scope) DO UPDATE
                         SET initial_delay = ($3 :: int4),
                             expires_at = now() + make_interval(secs => $3 :: int4)
       |]

lookupThrottle ::
  (PGConstraints r) =>
  Key ->
  Scope ->
  Sem r (Maybe RetryAfter)
lookupThrottle key scope =
  RetryAfter . fromIntegral <$$> runStatement (key, scope) selectThrottleStatement

selectThrottleStatement :: Hasql.Statement (Key, Scope) (Maybe Int32)
selectThrottleStatement =
  dimapPG
    [maybeStatement|SELECT (ceil(extract(epoch from (expires_at - now()))) :: int4)
                    FROM vcodes_throttle
                    WHERE key = ($1 :: text) AND scope = ($2 :: int4) AND expires_at > now()
                   |]
