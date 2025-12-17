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
import Wire.ActivationCodeStore (ActivationCodeStore (..))
import Wire.UserKeyStore

emailKeyToCode :: EmailKey -> ActivationCode
emailKeyToCode =
  ActivationCode
    . Ascii.unsafeFromText
    . pack
    . printf "%06d"
    . length
    . show

inMemoryActivationCodeStoreInterpreter ::
  ( Member (State (Map EmailKey (Maybe UserId, ActivationCode))) r
  ) =>
  InterpreterFor ActivationCodeStore r
inMemoryActivationCodeStoreInterpreter = interpret \case
  LookupActivationCode ek -> gets (!? ek)
  NewActivationCode ek _ uid -> do
    let key =
          ActivationKey
            . Ascii.encodeBase64Url
            . T.encodeUtf8
            . emailKeyUniq
            $ ek
        c = emailKeyToCode ek
    modify (insert ek (uid, c)) $> Activation key c
