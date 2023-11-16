{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.FederationConfigStore where

import Data.Domain
import Data.Id
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig qualified as API
import Wire.API.User.Search (FederatedUserSearchPolicy)

data FederationRestriction = FederationRestrictionAllowAll | FederationRestrictionByTeam [TeamId]
  deriving (Eq, Show, Ord)

data FederationDomainConfig = FederationDomainConfig
  { domain :: Domain,
    searchPolicy :: FederatedUserSearchPolicy,
    restriction :: FederationRestriction
  }

data FederationConfigStore m a where
  GetFederationConfig :: Domain -> FederationConfigStore m FederationDomainConfig
  GetFederationConfigs :: FederationConfigStore m [FederationDomainConfig]
  AddFederationConfig :: API.FederationDomainConfig -> FederationConfigStore m ()
  UpdateFederationConfig :: API.FederationDomainConfig -> FederationConfigStore m Bool
  AddFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  RemoveFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()

makeSem ''FederationConfigStore
