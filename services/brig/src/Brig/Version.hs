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

module Brig.Version where

import Brig.API.Handler
import Brig.App
import Data.Set qualified as Set
import Imports
import Servant (ServerT)
import Wire.API.Routes.Named
import Wire.API.Routes.Version

versionAPI :: ServerT VersionAPI (Handler r)
versionAPI = Named @"get-version" $ do
  fed <- asks (.federator)
  dom <- viewFederationDomain
  disabled <- asks (.disabledVersions)
  let allVersions = Set.difference (Set.fromList supportedVersions) disabled
      devVersions = Set.difference (Set.fromList developmentVersions) disabled
      supported = Set.difference allVersions devVersions
  pure $
    VersionInfo
      { vinfoSupported = VersionNumber <$> toList supported,
        vinfoDevelopment = VersionNumber <$> toList devVersions,
        vinfoFederation = isJust fed,
        vinfoDomain = dom
      }
