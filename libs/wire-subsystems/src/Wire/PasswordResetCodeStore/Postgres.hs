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
{-# LANGUAGE RecordWildCards #-}

module Wire.PasswordResetCodeStore.Postgres
  ( interpretPasswordResetCodeStoreToPostgres,
    genEmailCode,
    genPhoneCode,
  )
where

import Data.Id (UserId)
import Data.Text (pack)
import Data.Text.Ascii (encodeBase64Url, unsafeFromText)
import Data.Time.Clock (UTCTime)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.Random (randBytes)
import Polysemy
import Text.Printf (printf)
import Wire.API.PostgresMarshall
import Wire.API.User.Password
import Wire.PasswordResetCodeStore (PasswordResetCodeStore (..), PRQueryData (..))
import Wire.Postgres

interpretPasswordResetCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor PasswordResetCodeStore r
interpretPasswordResetCodeStoreToPostgres = interpret $ \case
  GenerateEmailCode -> genEmailCode
  GeneratePhoneCode -> genPhoneCode
  CodeSelect prk -> codeSelect prk
  CodeInsert prk prqd ttl -> codeInsert prk prqd ttl
  CodeDelete prk -> codeDelete prk

-- | 24 random bytes, base64url-encoded (mirrors the Cassandra interpreter).
genEmailCode :: (Member (Embed IO) r) => Sem r PasswordResetCode
genEmailCode = PasswordResetCode . encodeBase64Url <$> embed @IO (randBytes 24)

-- | A 6-digit, zero-padded code (mirrors the Cassandra interpreter).
genPhoneCode :: (Member (Embed IO) r) => Sem r PasswordResetCode
genPhoneCode =
  PasswordResetCode . unsafeFromText . pack . printf "%06d"
    <$> embed @IO (randIntegerZeroToNMinusOne 1000000)

codeSelect ::
  (PGConstraints r) =>
  PasswordResetKey ->
  Sem r (Maybe (PRQueryData Maybe))
codeSelect prk = do
  mRow <- runStatement prk select
  pure $ fmap toRecord mRow
  where
    toRecord ::
      (PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime) ->
      PRQueryData Maybe
    toRecord (prqdCode, prqdUser, prqdRetries, prqdTimeout) =
      PRQueryData {..}
    select ::
      Hasql.Statement PasswordResetKey (Maybe (PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime))
    select =
      dimapPG
        [maybeStatement|SELECT (code :: text), ("user" :: uuid), (retries :: int4?), (timeout :: timestamptz?)
                        FROM password_reset
                        WHERE key = ($1 :: text) AND expires_at > now ()
                       |]

codeInsert ::
  (PGConstraints r) =>
  PasswordResetKey ->
  PRQueryData Identity ->
  Int32 ->
  Sem r ()
codeInsert prk (PRQueryData prc uid n ut) ttl =
  runStatement (prk, prc, uid, Just (runIdentity n), Just (runIdentity ut), ttl) insert
  where
    insert ::
      Hasql.Statement (PasswordResetKey, PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime, Int32) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO password_reset (key, code, "user", retries, timeout, expires_at)
                             VALUES
                               ($1 :: text, $2 :: text, $3 :: uuid, $4 :: int4?, $5 :: timestamptz?, now() + make_interval(secs => $6 :: int))
                             ON CONFLICT (key) DO UPDATE
                             SET code = ($2 :: text),
                                 "user" = ($3 :: uuid),
                                 retries = ($4 :: int4?),
                                 timeout = ($5 :: timestamptz?),
                                 expires_at = now() + make_interval(secs => $6 :: int)
        |]

codeDelete ::
  (PGConstraints r) =>
  PasswordResetKey ->
  Sem r ()
codeDelete prk =
  runStatement prk delete
  where
    delete :: Hasql.Statement PasswordResetKey ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM password_reset
                             WHERE key = ($1 :: text)
                            |]
