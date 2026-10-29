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

module Wire.MockInterpreters.ActivationCodeStore where

import Data.Id
import Data.Map
import Data.Text (pack)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Imports hiding ((!?))
import Polysemy
import Polysemy.State
import Text.Printf (printf)
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.ActivationCodeStore
  ( ActivationCodeStore (..),
    ActivationKeyRow (..),
    maxAttempts,
  )
import Wire.UserKeyStore

emailKeyToCode :: EmailKey -> ActivationCode
emailKeyToCode =
  ActivationCode
    . Ascii.unsafeFromText
    . pack
    . printf "%06d"
    . length
    . show

-- | Derive the 'ActivationKey' exactly as 'NewActivationCode' does below.
-- (Intentionally NOT the SHA-256 derivation of 'mkActivationKey'; the mock
-- only needs internal consistency.)
mockKey :: EmailKey -> ActivationKey
mockKey = ActivationKey . Ascii.encodeBase64Url . T.encodeUtf8 . emailKeyUniq

inMemoryActivationCodeStoreInterpreter ::
  (Member (State (Map EmailKey (Maybe UserId, ActivationCode))) r) =>
  InterpreterFor ActivationCodeStore r
inMemoryActivationCodeStoreInterpreter =
  interpret \case
    LookupActivationCode ek -> gets (!? ek)
    NewActivationCode ek _ uid -> do
      let key = mockKey ek
          c = emailKeyToCode ek
      modify (insert ek (uid, c)) $> Activation key c
    DeleteActivationCode ek -> modify (delete ek)
    LookupActivationKey key -> do
      m <- get
      pure $
        listToMaybe
          [ ActivationKeyRow "email" (fromEmail (emailKeyOrig ek)) c uid maxAttempts
          | (ek, (uid, c)) <- Data.Map.toList m,
            mockKey ek == key
          ]
    -- The mock keeps no retry count.
    DecrementActivationRetries _ -> pure ()
    DeleteActivationKey key ->
      modify (Data.Map.filterWithKey (\ek _ -> mockKey ek /= key))
