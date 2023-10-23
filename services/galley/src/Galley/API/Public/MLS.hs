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

module Galley.API.Public.MLS where

import Galley.API.MLS
import Galley.App
import Wire.API.MakesFederatedCall
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.MLS

mlsAPI :: API MLSAPI GalleyEffects
mlsAPI =
  mkNamedAPI @"mls-message" (callsFed (exposeAnnotations postMLSMessageFromLocalUser))
    <@> mkNamedAPI @"mls-commit-bundle" (callsFed (exposeAnnotations postMLSCommitBundleFromLocalUser))
    <@> mkNamedAPI @"mls-public-keys" getMLSPublicKeys
