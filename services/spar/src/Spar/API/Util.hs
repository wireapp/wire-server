-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- | Swagger utilities.
module Spar.API.Util where

import Imports
import Servant
import Servant.Swagger

-- | A type-level tag that lets us omit any branch from Swagger docs.
data OmitDocs

instance HasSwagger (OmitDocs :> a) where
  toSwagger _ = mempty

instance HasServer api ctx => HasServer (OmitDocs :> api) ctx where
  type ServerT (OmitDocs :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- | A type family that prepares our API for docs generation.
--
-- Our Swagger docs are intended to be used by client developers, and not
-- all endpoints are useful for them or should be seen by them. Here we
-- assume the 'spar' service is only accessible from behind the 'nginz'
-- proxy, which handles authorization (adding a Z-User header if requests
-- are authorized)
--
-- We also omit all branches marked with 'OmitDocs'. Those are likely to be:
--
--   * Endpoints for which we can't generate Swagger docs.
--   * The endpoint that serves Swagger docs.
--   * Internal endpoints.
type family OutsideWorld (api :: k) :: k where
-- special cases
  OutsideWorld (Header "Z-User" a :> b) = OutsideWorld b
  OutsideWorld (OmitDocs :> b) = EmptyAPI
-- recursion
  OutsideWorld (a :<|> b) = OutsideWorld a :<|> OutsideWorld b
  OutsideWorld (a :> b) = a :> OutsideWorld b
  OutsideWorld x = x
