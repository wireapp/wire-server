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

-- | See also: 'DomainsBlockedForRegistration'.
module Galley.API.CustomBackend
  ( getCustomBackendByDomain,
  )
where

import Data.Domain (Domain)
import Galley.Effects.CustomBackendStore
import Imports hiding ((\\))
import Polysemy
import Wire.API.CustomBackend qualified as Public
import Wire.API.Error
import Wire.API.Error.Galley

-- PUBLIC ---------------------------------------------------------------------

getCustomBackendByDomain ::
  ( Member CustomBackendStore r,
    Member (ErrorS 'CustomBackendNotFound) r
  ) =>
  Domain ->
  Sem r Public.CustomBackend
getCustomBackendByDomain domain =
  getCustomBackend domain >>= \case
    Nothing -> throwS @'CustomBackendNotFound
    Just customBackend -> pure customBackend
