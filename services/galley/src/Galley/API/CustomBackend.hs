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
  ( getCustomBackendByDomainH,
    internalPutCustomBackendByDomainH,
    internalDeleteCustomBackendByDomainH,
  )
where

import Data.Domain (Domain)
import Galley.API.Util
import Galley.Effects.CustomBackendStore
import Galley.Effects.WaiRoutes
import Galley.Types
import Imports hiding ((\\))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, setStatus)
import Network.Wai.Utilities hiding (Error)
import Polysemy
import qualified Wire.API.CustomBackend as Public
import Wire.API.Error
import Wire.API.Error.Galley

-- PUBLIC ---------------------------------------------------------------------

getCustomBackendByDomainH ::
  Members
    '[ CustomBackendStore,
       ErrorS 'CustomBackendNotFound
     ]
    r =>
  Domain ::: JSON ->
  Sem r Response
getCustomBackendByDomainH (domain ::: _) =
  json <$> getCustomBackendByDomain domain

getCustomBackendByDomain ::
  Members '[CustomBackendStore, ErrorS 'CustomBackendNotFound] r =>
  Domain ->
  Sem r Public.CustomBackend
getCustomBackendByDomain domain =
  getCustomBackend domain >>= \case
    Nothing -> throwS @'CustomBackendNotFound
    Just customBackend -> pure customBackend

-- INTERNAL -------------------------------------------------------------------

internalPutCustomBackendByDomainH ::
  Members '[CustomBackendStore, WaiRoutes] r =>
  Domain ::: JsonRequest CustomBackend ->
  Sem r Response
internalPutCustomBackendByDomainH (domain ::: req) = do
  customBackend <- fromJsonBody req
  -- simple enough to not need a separate function
  setCustomBackend domain customBackend
  pure (empty & setStatus status201)

internalDeleteCustomBackendByDomainH :: Member CustomBackendStore r => Domain ::: JSON -> Sem r Response
internalDeleteCustomBackendByDomainH (domain ::: _) = do
  deleteCustomBackend domain
  pure (empty & setStatus status200)
