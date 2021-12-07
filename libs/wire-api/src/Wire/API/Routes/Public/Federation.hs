-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

-- | The publicly-facing API for federation
module Wire.API.Routes.Public.Federation where

import Data.Aeson (ToJSON)
import Data.Schema
import qualified Data.Swagger as S
import GHC.TypeLits
import Imports
import Servant
import Servant.API.Generic
import Servant.Swagger (toSwagger)

-- | Version of Wire's Federation API
--
-- Two releases of Wire are fully compatible in all federated features if they
-- share the same federation version.
--
-- FUTUREWORK: Define a compatability matrix when version 2 is released.
type FederationVersion = 1

federationVersion :: forall (v :: Nat) a. (KnownNat v, Num a) => a
federationVersion = fromInteger . natVal $ Proxy @v

data FederationVersionResponse (v :: Nat) = FederationVersionResponse
  deriving (ToJSON, S.ToSchema) via Schema (FederationVersionResponse v)

instance KnownNat v => ToSchema (FederationVersionResponse v) where
  schema =
    object "FederationVersionResponse" $
      FederationVersionResponse
        <$ const (federationVersion @v) .= field "version" (schema @Word)

data Api routes = Api
  { getAPIVersion ::
      routes
        :- Summary "Get the Wire API version"
        :> "api"
        :> "federation"
        :> "version"
        :> Get '[Servant.JSON] (FederationVersionResponse FederationVersion)
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

swaggerDoc :: S.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
