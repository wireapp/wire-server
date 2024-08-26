{-# LANGUAGE TemplateHaskell #-}

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

-- TODO: This store effect is more than just a store, we should break it up in
-- business logic and store
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
