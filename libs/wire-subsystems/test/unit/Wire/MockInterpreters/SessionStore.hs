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

module Wire.MockInterpreters.SessionStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User.Auth
import Wire.SessionStore

runInMemorySessionStore :: InterpreterFor SessionStore r
runInMemorySessionStore =
  evalState (mempty :: Map UserId [Cookie ()])
    . inMemorySessionStoreInterpreter
    . raiseUnder

inMemorySessionStoreInterpreter :: (Member (State (Map UserId [Cookie ()])) r) => InterpreterFor SessionStore r
inMemorySessionStoreInterpreter = interpret $ \case
  InsertCookie uid cookie _ttl -> modify $ Map.insertWith (<>) uid [cookie]
  ListCookies uid -> gets (Map.findWithDefault [] uid)
  DeleteAllCookies uid -> modify $ Map.delete uid
  DeleteCookies uid cc -> (error "implement on demand") uid cc
  LookupCookie uid time cid -> (error "implement on demand") uid time cid
