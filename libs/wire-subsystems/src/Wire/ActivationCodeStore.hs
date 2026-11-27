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
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.ActivationCodeStore
  ( ActivationCodeStore (..),
    lookupActivationCode,
    newActivationCode,
    deleteActivationCode,
    checkActivationCode,
    deleteActivationKey,
    ActivationKeyRow (..),
    ActivationCodeCheck (..),
    mkActivationKey,
    genActivationCode,
    maxAttempts,
  )
where

import Data.Id
import Data.Text (pack)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest
import Polysemy
import Text.Printf (printf)
import Util.Timeout
import Wire.API.User.Activation
import Wire.UserKeyStore

-- | What an activation key row is about: the activation target (as stored
-- key type and text) and the associated user, if any.
data ActivationKeyRow = ActivationKeyRow
  { keyType :: Text,
    keyText :: Text,
    user :: Maybe UserId
  }
  deriving (Eq, Show)

-- | Outcome of 'CheckActivationCode'.
data ActivationCodeCheck
  = -- | There is no (unexpired) row for the key.
    ActivationKeyNotFound
  | -- | The code matches; the row is left unchanged.
    ActivationCodeMatches ActivationKeyRow
  | -- | The code does not match; one retry has been consumed.
    ActivationCodeMismatch
  | -- | The code does not match and no retries were left; the row should be
    -- deleted with 'DeleteActivationKey'.
    ActivationRetriesExhausted
  deriving (Eq, Show)

data ActivationCodeStore :: Effect where
  LookupActivationCode ::
    EmailKey ->
    ActivationCodeStore m (Maybe (Maybe UserId, ActivationCode))
  -- | Create a code for a new pending activation for a given 'EmailKey'
  NewActivationCode ::
    EmailKey ->
    -- | The timeout for the activation code.
    Timeout ->
    -- | The user with whom to associate the activation code.
    Maybe UserId ->
    ActivationCodeStore m Activation
  -- | Delete a pending activation code for a given 'EmailKey', if any.
  -- This is used to invalidate a pending email-address update (e.g. when a
  -- user is put under SCIM control).
  DeleteActivationCode ::
    EmailKey ->
    ActivationCodeStore m ()
  -- | Check a code against the (unexpired) row of an opaque 'ActivationKey'.
  -- A mismatch consumes one retry, preserving the expiry; a mismatch with no
  -- retries left yields 'ActivationRetriesExhausted'.
  --
  -- The Postgres interpreter compares and consumes the retry in a single
  -- statement, so concurrent attempts cannot exceed the retry limit. The
  -- Cassandra interpreter reads, then writes, so concurrent attempts may be
  -- checked against the same retry count.
  CheckActivationCode ::
    ActivationKey ->
    ActivationCode ->
    ActivationCodeStore m ActivationCodeCheck
  -- | Delete the row for an opaque 'ActivationKey' (brute-force exhaustion).
  DeleteActivationKey ::
    ActivationKey ->
    ActivationCodeStore m ()

makeSem ''ActivationCodeStore

--------------------------------------------------------------------------------
-- Shared utilities (used by the Cassandra and Postgres interpreters)

-- | Compute the opaque 'ActivationKey' (SHA-256 hash, base64url-encoded) for
-- a given 'EmailKey'.
mkActivationKey :: EmailKey -> IO ActivationKey
mkActivationKey k = do
  d <- getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") pure d
  let bs = digestBS d' (T.encodeUtf8 $ emailKeyUniq k)
  pure . ActivationKey $ Ascii.encodeBase64Url bs

-- | Generate a fresh random 6-digit 'ActivationCode'.
genActivationCode :: IO ActivationCode
genActivationCode =
  ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
    <$> randIntegerZeroToNMinusOne 1000000

-- | Maximum number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3
