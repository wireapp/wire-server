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

module Galley.API.Clients
  ( getClientsH,
    addClientH,
    rmClientH,
  )
where

import Data.Id
import Galley.Effects
import qualified Galley.Effects.BrigAccess as E
import qualified Galley.Effects.ClientStore as E
import Galley.Types.Clients (clientIds, fromUserClients)
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities
import Polysemy

getClientsH ::
  Members '[BrigAccess, ClientStore] r =>
  UserId ->
  Sem r Response
getClientsH usr = do
  json <$> getClients usr

getClients ::
  Members '[BrigAccess, ClientStore] r =>
  UserId ->
  Sem r [ClientId]
getClients usr = do
  isInternal <- E.useIntraClientListing
  clts <-
    if isInternal
      then fromUserClients <$> E.lookupClients [usr]
      else E.getClients [usr]
  return $ clientIds usr clts

addClientH ::
  Member ClientStore r =>
  UserId ::: ClientId ->
  Sem r Response
addClientH (usr ::: clt) = do
  E.createClient usr clt
  return empty

rmClientH ::
  Member ClientStore r =>
  UserId ::: ClientId ->
  Sem r Response
rmClientH (usr ::: clt) = do
  E.deleteClient usr clt
  return empty
