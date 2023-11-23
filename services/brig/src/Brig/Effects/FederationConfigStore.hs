{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.FederationConfigStore where

import Data.Domain
import Data.Id
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig

data AddFederationRemoteResult = AddFederationRemoteSuccess | AddFederationRemoteMaxRemotesReached

data FederationConfigStore m a where
  GetFederationConfig :: Domain -> FederationConfigStore m (Maybe FederationDomainConfig)
  GetFederationConfigs :: FederationConfigStore m [FederationDomainConfig]
  AddFederationConfig :: FederationDomainConfig -> FederationConfigStore m AddFederationRemoteResult
  UpdateFederationConfig :: FederationDomainConfig -> FederationConfigStore m Bool
  AddFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  RemoveFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  GetFederationRemoteTeams :: Domain -> FederationConfigStore m [FederationRemoteTeam]

makeSem ''FederationConfigStore
