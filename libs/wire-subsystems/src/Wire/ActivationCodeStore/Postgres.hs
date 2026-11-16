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

module Wire.ActivationCodeStore.Postgres
  ( interpretActivationCodeStoreToPostgres,
  )
where

import Data.Id (UserId)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Util.Timeout
import Wire.API.PostgresMarshall
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.ActivationCodeStore
import Wire.Postgres
import Wire.UserKeyStore

interpretActivationCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor ActivationCodeStore r
interpretActivationCodeStoreToPostgres = interpret $ \case
  LookupActivationCode ek -> do
    key <- embed (mkActivationKey ek)
    runStatement key lookupCode
  NewActivationCode ek timeout uid -> newActivationCodeImpl ek timeout uid
  DeleteActivationCode ek -> do
    key <- embed (mkActivationKey ek)
    runStatement key deleteCode
  CheckActivationCode key code -> do
    mRow <- runStatement (key, code) checkCode
    pure $ case mRow of
      Nothing -> ActivationKeyNotFound
      Just (keyType, keyText, user, matches, retriesLeft)
        | matches -> ActivationCodeMatches (ActivationKeyRow keyType keyText user)
        | retriesLeft >= 0 -> ActivationCodeMismatch
        | otherwise -> ActivationRetriesExhausted
  DeleteActivationKey key -> runStatement key deleteCode

lookupCode :: Hasql.Statement ActivationKey (Maybe (Maybe UserId, ActivationCode))
lookupCode =
  dimapPG
    [maybeStatement|
      SELECT user_id :: uuid?, code :: text
      FROM activation_keys
      WHERE key = ($1 :: text) AND expires_at > now()
    |]

newActivationCodeImpl ::
  (PGConstraints r) =>
  EmailKey ->
  Timeout ->
  Maybe UserId ->
  Sem r Activation
newActivationCodeImpl ek timeout u = do
  key <- embed (mkActivationKey ek)
  code <- embed genActivationCode
  let keyText = fromEmail (emailKeyOrig ek)
  runStatement (key, "email", keyText, code, u, maxAttempts, round timeout) insertWithTtl
  pure $ Activation key code

--------------------------------------------------------------------------------
-- Statements

insertWithTtl ::
  Hasql.Statement (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
insertWithTtl =
  lmapPG
    [resultlessStatement|
      INSERT INTO activation_keys (key, key_type, key_text, code, user_id, retries, expires_at)
      VALUES ($1 :: text, $2 :: text, $3 :: text, $4 :: text, $5 :: uuid?, $6 :: int4, now() + make_interval(secs => $7 :: int4))
      ON CONFLICT (key) DO UPDATE
      SET key_type = ($2 :: text),
          key_text = ($3 :: text),
          code = ($4 :: text),
          user_id = ($5 :: uuid?),
          retries = ($6 :: int4),
          expires_at = now() + make_interval(secs => $7 :: int4)
    |]

-- | Compare the code and, on a mismatch, consume one retry, all in a single
-- statement: the row lock serialises concurrent attempts, so at most
-- 'maxAttempts' + 1 mismatching codes are ever compared. A mismatch with no
-- retries left drives the counter to -1, which makes the row invisible to
-- further checks until it is deleted.
checkCode ::
  Hasql.Statement (ActivationKey, ActivationCode) (Maybe (Text, Text, Maybe UserId, Bool, Int32))
checkCode =
  dimapPG
    [maybeStatement|
      UPDATE activation_keys
      SET retries = CASE WHEN code = ($2 :: text) THEN retries ELSE retries - 1 END
      WHERE key = ($1 :: text) AND retries >= 0 AND expires_at > now()
      RETURNING key_type :: text, key_text :: text, user_id :: uuid?, (code = ($2 :: text)) :: bool, retries :: int4
    |]

deleteCode :: Hasql.Statement ActivationKey ()
deleteCode =
  lmapPG
    [resultlessStatement|
      DELETE FROM activation_keys WHERE key = ($1 :: text)
    |]
