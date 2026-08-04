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
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
    verifyActivationCode,
    mkActivationKey,
    genActivationCode,
    maxAttempts,
    mkActivationScope,
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
import Wire.API.User.EmailAddress
import Wire.UserKeyStore

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
  -- | Verify an activation code against the stored value for the given key.
  -- On a match, returns the associated 'EmailKey' and 'UserId'.  On a
  -- mismatch with remaining retries, decrements the retry counter (preserving
  -- the remaining TTL).  On exhaustion, deletes the row.  Returns 'Nothing'
  -- for any non-matching outcome (the caller treats this as an invalid code).
  VerifyActivationCode ::
    ActivationKey ->
    ActivationCode ->
    ActivationCodeStore m (Maybe (EmailKey, Maybe UserId))

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

-- | Reconstruct an activation scope from the stored key type/text.
-- Returns 'Just' if the key type is @"email"@ and the text parses as an
-- email address; 'Nothing' otherwise.
mkActivationScope :: Text -> Text -> Maybe UserId -> Maybe (EmailKey, Maybe UserId)
mkActivationScope "email" keyText mUser =
  case emailAddressText keyText of
    Just e -> Just (mkEmailKey e, mUser)
    Nothing -> Nothing
mkActivationScope _ _ _ = Nothing
