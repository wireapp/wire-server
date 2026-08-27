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
    lookupActivationKey,
    decrementActivationRetries,
    deleteActivationKey,
    ActivationKeyRow (..),
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

-- | Persisted state of one activation key row (no TTL/expiry exposure;
-- expiry handling is a storage-internal concern).
data ActivationKeyRow = ActivationKeyRow
  { keyType :: Text,
    keyText :: Text,
    code :: ActivationCode,
    user :: Maybe UserId,
    retries :: Int32
  }

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
  -- | Read the full row for an opaque 'ActivationKey' (unexpired only).
  LookupActivationKey ::
    ActivationKey ->
    ActivationCodeStore m (Maybe ActivationKeyRow)
  -- | Decrement the retry counter by one, preserving expiry.
  -- No-op when the row is absent or already at 0.
  DecrementActivationRetries ::
    ActivationKey ->
    ActivationCodeStore m ()
  -- | Delete the row for an opaque 'ActivationKey' (brute-force exhaustion).
  DeleteActivationKey ::
    ActivationKey ->
    ActivationCodeStore m ()

makeSem ''ActivationCodeStore

--------------------------------------------------------------------------------
-- Shared utilities (used by Cassandra, Postgres, DualWrite interpreters)

-- | Compute the opaque 'ActivationKey' (SHA-256 hash, base64url-encoded) for
-- a given 'EmailKey'.  Moved here from the Cassandra interpreter so that all
-- interpreters share a single definition.
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
