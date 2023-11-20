{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.FederationConfigStore where

import Data.Domain
import Data.Id
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig qualified as API
import Wire.API.User.Search (FederatedUserSearchPolicy)

data FederationRestriction = FederationRestrictionAllowAll | FederationRestrictionByTeam [TeamId]
  deriving stock (Eq, Show, Ord)

data AddFederationRemoteResult = AddFederationRemoteSuccess | AddFederationRemoteMaxRemotesReached

data FederationDomainConfig = FederationDomainConfig
  { domain :: Domain,
    searchPolicy :: FederatedUserSearchPolicy,
    restriction :: FederationRestriction
  }
  deriving stock (Show)

fromFederationDomainConfig :: FederationDomainConfig -> API.FederationDomainConfig
fromFederationDomainConfig (FederationDomainConfig d p FederationRestrictionAllowAll) = API.FederationDomainConfig d p API.FederationRestrictionAllowAll
fromFederationDomainConfig (FederationDomainConfig d p (FederationRestrictionByTeam _)) = API.FederationDomainConfig d p API.FederationRestrictionByTeam

data FederationConfigStore m a where
  GetFederationConfig :: Domain -> FederationConfigStore m (Maybe FederationDomainConfig)
  GetFederationConfigs :: FederationConfigStore m [FederationDomainConfig]
  AddFederationConfig :: API.FederationDomainConfig -> FederationConfigStore m AddFederationRemoteResult
  UpdateFederationConfig :: API.FederationDomainConfig -> FederationConfigStore m Bool
  AddFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  RemoveFederationRemoteTeam :: Domain -> TeamId -> FederationConfigStore m ()
  GetFederationRemoteTeams :: Domain -> FederationConfigStore m [API.FederationRemoteTeam]

makeSem ''FederationConfigStore
