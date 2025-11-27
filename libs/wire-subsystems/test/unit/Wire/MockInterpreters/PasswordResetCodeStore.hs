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

module Wire.MockInterpreters.PasswordResetCodeStore where

import Data.Map qualified as Map
import Data.Text.Ascii
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User.Password
import Wire.PasswordResetCodeStore

runInMemoryPasswordResetCodeStore :: forall r. InterpreterFor PasswordResetCodeStore r
runInMemoryPasswordResetCodeStore =
  evalState (mempty :: Map PasswordResetKey (PRQueryData Identity))
    . inMemoryPasswordResetCodeStore
    . raiseUnder

inMemoryPasswordResetCodeStore ::
  forall r.
  (Member (State (Map PasswordResetKey (PRQueryData Identity))) r) =>
  InterpreterFor PasswordResetCodeStore r
inMemoryPasswordResetCodeStore =
  interpret
    \case
      GenerateEmailCode ->
        pure . PasswordResetCode . encodeBase64Url $ "email-code"
      GeneratePhoneCode -> (error "deprecated")
      CodeSelect resetKey -> do
        gets $
          fmap (mapPRQueryData (Just . runIdentity))
            . Map.lookup resetKey
      CodeInsert resetKey queryData _ttl -> do
        modify $ Map.insert resetKey queryData
      CodeDelete resetKey -> do
        modify $ Map.delete resetKey
