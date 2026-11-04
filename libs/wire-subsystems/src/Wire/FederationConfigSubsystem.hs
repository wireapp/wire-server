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

module Wire.FederationConfigSubsystem where

import Data.Domain (Domain)
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig
import Wire.FederationConfigStore qualified as Store

data FederationConfigSubsystem m a where
  GetFederationConfig :: Domain -> FederationConfigSubsystem m (Maybe FederationDomainConfig)
  GetFederationConfigs :: FederationConfigSubsystem m FederationDomainConfigs
  AddFederationConfig :: FederationDomainConfig -> FederationConfigSubsystem m Store.AddFederationRemoteResult
  UpdateFederationConfig :: FederationDomainConfig -> FederationConfigSubsystem m Store.UpdateFederationResult
  AddFederationRemoteTeam :: Domain -> TeamId -> FederationConfigSubsystem m Store.AddFederationRemoteTeamResult
  RemoveFederationRemoteTeam :: Domain -> TeamId -> FederationConfigSubsystem m ()
  GetFederationRemoteTeams :: Domain -> FederationConfigSubsystem m [FederationRemoteTeam]
  -- | Check if the local backend federates with a remote team.
  BackendFederatesWith :: Remote (Maybe TeamId) -> FederationConfigSubsystem m Bool

makeSem ''FederationConfigSubsystem
