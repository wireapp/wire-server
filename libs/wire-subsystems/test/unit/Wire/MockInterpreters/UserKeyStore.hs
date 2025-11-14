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

module Wire.MockInterpreters.UserKeyStore where

import Data.Id
import Data.Map qualified as M
import Imports
import Polysemy
import Polysemy.State
import Wire.UserKeyStore

inMemoryUserKeyStoreInterpreter ::
  (Member (State (Map EmailKey UserId)) r) =>
  InterpreterFor UserKeyStore r
inMemoryUserKeyStoreInterpreter = interpret $ \case
  LookupKey key -> do
    gets (M.lookup key)
  InsertKey uid key ->
    modify $ M.insert key uid
  DeleteKey key ->
    modify $ M.delete key
  DeleteKeyForUser uid key ->
    modify $ M.filterWithKey (\k u -> k /= key && u /= uid)
  ClaimKey key uid -> do
    keys <- get
    let free = M.notMember key keys || M.lookup key keys == (Just uid)
    when free $
      modify $
        M.insert key uid
    pure free
  KeyAvailable key uid -> do
    keys <- get
    pure $ M.notMember key keys || M.lookup key keys == uid
