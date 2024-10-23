-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

-- | Servant combinators related to Swagger docs
module Wire.API.SwaggerServant
  ( OmitDocs,
  )
where

import Data.Metrics.Servant
import Data.Proxy
import Servant
import Wire.API.Routes.Version

-- | A type-level tag that lets us omit any branch from Swagger docs.
--
-- FUTUREWORK(fisx): this is currently only used for the spar internal api
-- and spar scim, and we should probably eliminate those uses and this combinator.
-- it's only justification is laziness.
data OmitDocs

instance (HasServer api ctx) => HasServer (OmitDocs :> api) ctx where
  type ServerT (OmitDocs :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance (RoutesToPaths api) => RoutesToPaths (OmitDocs :> api) where
  getRoutes = getRoutes @api

type instance
  SpecialiseToVersion v (OmitDocs :> api) = EmptyAPI
