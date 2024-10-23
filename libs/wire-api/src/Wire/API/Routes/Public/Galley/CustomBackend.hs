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

module Wire.API.Routes.Public.Galley.CustomBackend where

import Data.Domain
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.CustomBackend
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.Named

type CustomBackendAPI =
  Named
    "get-custom-backend-by-domain"
    ( Summary "Shows information about custom backends related to a given email domain"
        :> CanThrow 'CustomBackendNotFound
        :> "custom-backend"
        :> "by-domain"
        :> Capture' '[Description "URL-encoded email domain"] "domain" Domain
        :> Get '[JSON] CustomBackend
    )
