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
    FederationStrategy (..),
    defFederationDomainConfigs,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Domain (Domain)
import Data.Schema
import qualified Data.Swagger as S
import GHC.Generics
import Imports
import Wire.API.User.Search (FederatedUserSearchPolicy)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-- | Everything we need to know about a remote instance in order to federate with it.  Comes
-- in `AllowedDomains` if `AllowStrategy` is `AllowList`.  If `AllowAll`, we still use this
-- information for search policy.
data FederationDomainConfig = FederationDomainConfig
  { domain :: Domain,
    cfgSearchPolicy :: FederatedUserSearchPolicy
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationDomainConfig
  deriving (Arbitrary) via (GenericUniform FederationDomainConfig)

instance ToSchema FederationDomainConfig where
  schema =
    object "FederationDomainConfig" $
      FederationDomainConfig
        <$> domain .= field "domain" schema
        <*> cfgSearchPolicy .= field "search_policy" schema

data FederationDomainConfigs = FederationDomainConfigs
  { strategy :: FederationStrategy,
    fromFederationDomainConfigs :: [FederationDomainConfig], -- TODO: rename to `remotes`
    updateInterval :: Int
  }
  deriving (Show, Generic, Eq)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationDomainConfigs
  deriving (Arbitrary) via (GenericUniform FederationDomainConfigs)

defFederationDomainConfigs :: FederationDomainConfigs
defFederationDomainConfigs =
  FederationDomainConfigs
    { strategy = AllowNone,
      fromFederationDomainConfigs = [],
      updateInterval = 10
    }

instance ToSchema FederationDomainConfigs where
  schema =
    object "FederationDomainConfigs" $
      FederationDomainConfigs
        <$> strategy .= field "strategy" schema
        <*> fromFederationDomainConfigs .= field "remotes" (array schema)
        <*> updateInterval .= field "updateInterval (seconds)" schema

data FederationStrategy
  = -- | Disable federation.
    AllowNone
  | -- | Allow any backend that asks.
    AllowAll
  | -- | Any backend explicitly configured in table `brig.federation_remotes` (if that table
    -- is empty, this is the same as `AllowNone`).
    AllowList
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationStrategy
  deriving (Arbitrary) via (GenericUniform FederationStrategy)

instance ToSchema FederationStrategy where
  schema =
    enum @Text "FederationStrategy" $
      mconcat
        [ element "allowNone" AllowNone,
          element "allowAll" AllowAll,
          element "allowList" AllowList
        ]
