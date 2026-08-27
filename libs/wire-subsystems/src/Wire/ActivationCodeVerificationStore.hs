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

module Wire.ActivationCodeVerificationStore
  ( ActivationCodeVerificationStore (..),
    verifyActivationCode,
    interpretActivationCodeVerificationStore,
  )
where

import Data.Id
import Imports
import Polysemy
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.ActivationCodeStore
import Wire.UserKeyStore

data ActivationCodeVerificationStore :: Effect where
  VerifyActivationCode ::
    ActivationKey ->
    ActivationCode ->
    ActivationCodeVerificationStore m (Maybe (EmailKey, Maybe UserId))

makeSem ''ActivationCodeVerificationStore

-- | Verify an activation code against the stored value.  On a match,
-- return the reconstructed scope.  On a mismatch with remaining retries,
-- decrement the counter.  On exhaustion, delete the row.  'Nothing' for
-- any non-matching outcome.
interpretActivationCodeVerificationStore ::
  (Member ActivationCodeStore r) =>
  InterpreterFor ActivationCodeVerificationStore r
interpretActivationCodeVerificationStore = interpret $ \case
  VerifyActivationCode key code -> do
    mRow <- lookupActivationKey key
    case mRow of
      Nothing -> pure Nothing
      Just row
        | row.code == code -> pure (mkActivationScope row.keyType row.keyText row.user)
        | row.retries >= 1 -> decrementActivationRetries key $> Nothing
        | otherwise -> deleteActivationKey key $> Nothing

-- | Reconstruct an activation scope from the stored key type/text.
-- Returns 'Just' if the key type is @"email"@ and the text parses as an
-- email address; 'Nothing' otherwise.
mkActivationScope :: Text -> Text -> Maybe UserId -> Maybe (EmailKey, Maybe UserId)
mkActivationScope "email" keyText mUser =
  case emailAddressText keyText of
    Just e -> Just (mkEmailKey e, mUser)
    Nothing -> Nothing
mkActivationScope _ _ _ = Nothing
