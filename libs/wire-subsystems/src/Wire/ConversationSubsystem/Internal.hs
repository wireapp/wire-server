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

module Wire.ConversationSubsystem.Internal (internalGetClientIds) where

import Data.Id
import Galley.Types.Clients (Clients, fromUserClients)
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Conversation.Config
import Wire.BrigAPIAccess
import Wire.UserClientIndexStore (UserClientIndexStore)
import Wire.UserClientIndexStore qualified as UserClientIndexStore

internalGetClientIds ::
  ( Member BrigAPIAccess r,
    Member UserClientIndexStore r,
    Member (Input ConversationSubsystemConfig) r
  ) =>
  [UserId] ->
  Sem r Clients
internalGetClientIds users = do
  isInternal <- inputs (.listClientsUsingBrig)
  if isInternal
    then fromUserClients <$> lookupClients users
    else UserClientIndexStore.getClients users
