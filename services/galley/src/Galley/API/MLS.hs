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

module Galley.API.MLS
  ( isMLSEnabled,
    assertMLSEnabled,
    postMLSMessage,
    postMLSCommitBundleFromLocalUser,
    postMLSMessageFromLocalUser,
    getMLSPublicKeys,
  )
where

import Control.Lens (view)
import Data.Id
import Data.Qualified
import Galley.API.MLS.Enabled
import Galley.API.MLS.Message
import Galley.Env
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Keys

getMLSPublicKeys ::
  ( Member (Input Env) r,
    Member (ErrorS 'MLSNotEnabled) r
  ) =>
  Local UserId ->
  Sem r MLSPublicKeys
getMLSPublicKeys _ = do
  assertMLSEnabled
  keys <- inputs (view mlsKeys)
  pure $ mlsKeysToPublic keys
