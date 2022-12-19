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
import Wire.API.Federation.API
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.MLS

mlsAPI ::
  ( CallsFed 'Galley "mls-welcome",
    CallsFed 'Brig "get-mls-clients",
    CallsFed 'Galley "on-conversation-updated",
    CallsFed 'Galley "on-mls-message-sent",
    CallsFed 'Galley "on-new-remote-conversation",
    CallsFed 'Galley "send-mls-message",
    CallsFed 'Galley "send-mls-commit-bundle"
  ) =>
  API MLSAPI GalleyEffects
mlsAPI =
  mkNamedAPI @"mls-welcome-message" postMLSWelcomeFromLocalUser
    <@> mkNamedAPI @"mls-message-v1" postMLSMessageFromLocalUserV1
    <@> mkNamedAPI @"mls-message" postMLSMessageFromLocalUser
    <@> mkNamedAPI @"mls-commit-bundle" postMLSCommitBundleFromLocalUser
    <@> mkNamedAPI @"mls-public-keys" getMLSPublicKeys
