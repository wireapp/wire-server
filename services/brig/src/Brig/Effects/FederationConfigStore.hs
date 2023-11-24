{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.FederationConfigStore where

import Data.Domain
import Data.Id
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
  | UpdateFederationRemoteDomainMismatch

data AddFederationRemoteTeamResult
  = AddFederationRemoteTeamSuccess
  | AddFederationRemoteTeamDomainNotFound
  | AddFederationRemoteTeamRestrictionAllowAll

data FederationConfigStore m a where
  GetFederationConfig :: Domain -> FederationConfigStore m (Maybe FederationDomainConfig)
  GetFederationConfigs :: FederationConfigStore m FederationDomainConfigs
  AddFederationConfig :: FederationDomainConfig -> FederationConfigStore m AddFederationRemoteResult
  UpdateFederationConfig :: Domain -> FederationDomainConfig -> FederationConfigStore m UpdateFederationResult
  AddFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m AddFederationRemoteTeamResult
  RemoveFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  GetFederationRemoteTeams :: Domain -> FederationConfigStore m [FederationRemoteTeam]

makeSem ''FederationConfigStore
