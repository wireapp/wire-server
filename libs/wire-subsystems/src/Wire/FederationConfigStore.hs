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

module Wire.FederationConfigStore where

import Data.Domain
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig

data AddFederationRemoteResult
  = AddFederationRemoteSuccess
  | AddFederationRemoteMaxRemotesReached
  | AddFederationRemoteDivergingConfig (Map Domain FederationDomainConfig)

data UpdateFederationResult
  = UpdateFederationSuccess
  | UpdateFederationRemoteNotFound
  | UpdateFederationRemoteDivergingConfig

data AddFederationRemoteTeamResult
  = AddFederationRemoteTeamSuccess
  | AddFederationRemoteTeamDomainNotFound
  | AddFederationRemoteTeamRestrictionAllowAll

-- FUTUREWORK: This store effect is more than just a store,
-- we should break it up in business logic and store
data FederationConfigStore m a where
  GetFederationConfig :: Domain -> FederationConfigStore m (Maybe FederationDomainConfig)
  GetFederationConfigs :: FederationConfigStore m FederationDomainConfigs
  AddFederationConfig :: FederationDomainConfig -> FederationConfigStore m AddFederationRemoteResult
  UpdateFederationConfig :: FederationDomainConfig -> FederationConfigStore m UpdateFederationResult
  AddFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m AddFederationRemoteTeamResult
  RemoveFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  GetFederationRemoteTeams :: Domain -> FederationConfigStore m [FederationRemoteTeam]
  -- | Check if the local backend federates with a remote team.
  BackendFederatesWith :: Remote (Maybe TeamId) -> FederationConfigStore m Bool

makeSem ''FederationConfigStore
