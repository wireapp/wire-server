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

module Wire.MockInterpreters.FederationConfigStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.API.Routes.FederationDomainConfig
import Wire.FederationConfigStore

inMemoryFederationConfigStoreInterpreter ::
  (Member (State [FederationDomainConfig]) r) =>
  InterpreterFor FederationConfigStore r
inMemoryFederationConfigStoreInterpreter =
  interpret $ \case
    GetFederationConfig domain -> gets $ find (\cfg -> cfg.domain == domain)
    GetFederationConfigs -> do
      remoteConfigs <- get
      pure $ FederationDomainConfigs AllowDynamic remoteConfigs 1
    AddFederationConfig newCfg -> do
      modify $ (newCfg :) . deleteBy (\a b -> a.domain == b.domain) newCfg
      pure AddFederationRemoteSuccess
    UpdateFederationConfig _ ->
      error "UpdateFederationConfig not implemented in inMemoryFederationConfigStoreInterpreter"
    AddFederationRemoteTeam _ _ ->
      error "AddFederationRemoteTeam not implemented in inMemoryFederationConfigStoreInterpreter"
    RemoveFederationRemoteTeam _ _ ->
      error "RemoveFederationRemoteTeam not implemented in inMemoryFederationConfigStoreInterpreter"
    GetFederationRemoteTeams _ ->
      error "GetFederationRemoteTeams not implemented in inMemoryFederationConfigStoreInterpreter"
    BackendFederatesWith _ ->
      error "BackendFederatesWith not implemented in inMemoryFederationConfigStoreInterpreter"

runFederationConfigStoreInMemory :: InterpreterFor FederationConfigStore r
runFederationConfigStoreInMemory =
  evalState []
    . inMemoryFederationConfigStoreInterpreter
    . raiseUnder
