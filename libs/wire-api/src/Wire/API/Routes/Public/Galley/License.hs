-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Galley.License where

import Servant
import Wire.API.License
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type LicenseAPI =
  Named
    "get-license"
    ( Summary "Get the deployment license"
        :> Description
             "Returns which licensed meeting features this deployment covers. Serves all-false flags when WIRE_LICENSE is unset, invalid, expired, or does not cover this domain."
        :> From 'V19
        :> ZLocalUser
        :> "license"
        :> MultiVerb1 'GET '[JSON] (Respond 200 "The deployment license" License)
    )
