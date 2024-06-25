{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Routes.FederationDomainConfig
  ( FederationDomainConfig (..),
    FederationDomainConfigs (..),
    defFederationDomainConfigs,
    FederationStrategy (..),
    FederationRemoteTeam (..),
    FederationRestriction (..),
  )
where

import Control.Lens (makePrisms, (?~))
import Control.Lens.Tuple (_1)
import Data.Aeson (FromJSON, ToJSON)
import Data.Domain (Domain)
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import GHC.Generics
import Imports
import Wire.API.User.Search (FederatedUserSearchPolicy)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

data FederationRestriction = FederationRestrictionAllowAll | FederationRestrictionByTeam [TeamId]
  deriving (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform FederationRestriction)

makePrisms ''FederationRestriction

data FederationRestrictionTag = FederationRestrictionAllowAllTag | FederationRestrictionByTeamTag
  deriving (Eq, Enum, Bounded)

deriving via Schema FederationRestriction instance (S.ToSchema FederationRestriction)

deriving via Schema FederationRestriction instance (FromJSON FederationRestriction)

deriving via Schema FederationRestriction instance (ToJSON FederationRestriction)

tagSchema :: ValueSchema NamedSwaggerDoc FederationRestrictionTag
tagSchema =
  enum @Text "FederationRestrictionTag" $
    mconcat [element "allow_all" FederationRestrictionAllowAllTag, element "restrict_by_team" FederationRestrictionByTeamTag]

instance ToSchema FederationRestriction where
  schema =
    object "FederationRestriction" $
      fromTagged
        <$> toTagged
          .= bind
            (fst .= field "tag" tagSchema)
            (snd .= fieldOver _1 "value" untaggedSchema)
    where
      toTagged :: FederationRestriction -> (FederationRestrictionTag, FederationRestriction)
      toTagged d@(FederationRestrictionAllowAll) = (FederationRestrictionAllowAllTag, d)
      toTagged d@(FederationRestrictionByTeam _) = (FederationRestrictionByTeamTag, d)

      fromTagged :: (FederationRestrictionTag, FederationRestriction) -> FederationRestriction
      fromTagged = snd

      untaggedSchema = dispatch $ \case
        FederationRestrictionAllowAllTag -> tag _FederationRestrictionAllowAll null_
        FederationRestrictionByTeamTag -> tag _FederationRestrictionByTeam (array schema)

-- | Everything we need to know about a remote instance in order to federate with it.  Comes
-- in `AllowedDomains` if `AllowStrategy` is `AllowDynamic`.  If `AllowAll`, we still use this
-- information for search policy.
data FederationDomainConfig = FederationDomainConfig
  { domain :: Domain,
    searchPolicy :: FederatedUserSearchPolicy,
    restriction :: FederationRestriction
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationDomainConfig
  deriving (Arbitrary) via (GenericUniform FederationDomainConfig)

instance ToSchema FederationDomainConfig where
  schema =
    object "FederationDomainConfig" $
      FederationDomainConfig
        <$> domain .= field "domain" schema
        <*> searchPolicy .= field "search_policy" schema
        <*> restriction .= field "restriction" schema

data FederationDomainConfigs = FederationDomainConfigs
  { strategy :: FederationStrategy,
    remotes :: [FederationDomainConfig],
    updateInterval :: Int
  }
  deriving (Show, Generic, Eq)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationDomainConfigs
  deriving (Arbitrary) via (GenericUniform FederationDomainConfigs)

defFederationDomainConfigs :: FederationDomainConfigs
defFederationDomainConfigs =
  FederationDomainConfigs
    { strategy = AllowNone,
      remotes = [],
      updateInterval = 10
    }

instance ToSchema FederationDomainConfigs where
  schema =
    objectWithDocModifier
      "FederationDomainConfigs"
      (description ?~ "See https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections.")
      $ FederationDomainConfigs
        <$> strategy .= field "strategy" schema
        <*> remotes .= field "remotes" (array schema)
        <*> updateInterval .= field "update_interval" schema

data FederationStrategy
  = -- | Disable federation.
    AllowNone
  | -- | Allow any backend that asks.
    AllowAll
  | -- | Any backend explicitly configured in table `brig.federation_remotes` (if that table
    -- is empty, this is the same as `AllowNone`).
    AllowDynamic
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationStrategy
  deriving (Arbitrary) via (GenericUniform FederationStrategy)

instance ToSchema FederationStrategy where
  schema =
    enum @Text "FederationStrategy" $
      mconcat
        [ element "allowNone" AllowNone,
          element "allowAll" AllowAll,
          element "allowDynamic" AllowDynamic
        ]

newtype FederationRemoteTeam = FederationRemoteTeam
  { teamId :: TeamId
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationRemoteTeam
  deriving (Arbitrary) via (GenericUniform FederationRemoteTeam)

instance ToSchema FederationRemoteTeam where
  schema =
    object "FederationRemoteTeam" $
      FederationRemoteTeam
        <$> teamId .= field "team_id" schema
