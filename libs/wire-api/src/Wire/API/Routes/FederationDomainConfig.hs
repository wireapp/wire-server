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
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Domain (Domain)
import Data.Schema
import qualified Data.Swagger as S
import GHC.Generics
import Imports
import Wire.API.User.Search (FederatedUserSearchPolicy)

-- | Everything we need to know about a remote instance in order to federate with it.  Comes
-- in `AllowedDomains` if `AllowStrategy` is `AllowList`.  If `AllowAll`, we still use this
-- information for search policy.
data FederationDomainConfig = FederationDomainConfig
  { domain :: Domain,
    cfgSearchPolicy :: FederatedUserSearchPolicy
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationDomainConfig

instance ToSchema FederationDomainConfig where
  schema =
    object "FederationDomainConfig" $
      FederationDomainConfig
        <$> domain .= field "domain" schema
        <*> cfgSearchPolicy .= field "search_policy" schema

data FederationDomainConfigs = FederationDomainConfigs
  { fromFederationDomainConfigs :: [FederationDomainConfig]
  , updateInterval :: Int
  }
  deriving (Show, Generic, Eq)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema FederationDomainConfigs

instance ToSchema FederationDomainConfigs where
  schema =
    object "FederationDomainConfigs" $
      FederationDomainConfigs
        <$> fromFederationDomainConfigs .= field "remotes" (array schema)
        <*> updateInterval .= field "updateInterval" schema