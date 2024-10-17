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

module Proxy.API.Internal
  ( InternalAPI,
    servantSitemap,
  )
where

import Imports hiding (head)
import Proxy.Proxy qualified
import Servant
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named (Named))

type InternalAPI = Named "status" ("i" :> "status" :> MultiVerb 'GET '[Servant.JSON] '[RespondEmpty 200 "OK"] ())

servantSitemap :: ServerT InternalAPI Proxy.Proxy.Proxy
servantSitemap = Named @"status" (pure ())
