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

module Galley.API.CustomBackend
  ( getCustomBackendByDomainH,
    internalPutCustomBackendByDomainH,
    internalDeleteCustomBackendByDomainH,
  )
where

import Control.Monad.Catch
import Data.Domain (Domain)
import Galley.API.Error
import Galley.API.Util
import Galley.App
import qualified Galley.Data.CustomBackend as Data
import Galley.Types
import Imports hiding ((\\))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities

-- PUBLIC ---------------------------------------------------------------------

getCustomBackendByDomainH :: Domain ::: JSON -> Galley Response
getCustomBackendByDomainH (domain ::: _) =
  json <$> getCustomBackendByDomain domain

getCustomBackendByDomain :: Domain -> Galley CustomBackend
getCustomBackendByDomain domain =
  Data.getCustomBackend domain >>= \case
    Nothing -> throwM (customBackendNotFound domain)
    Just customBackend -> pure customBackend

-- INTERNAL -------------------------------------------------------------------

internalPutCustomBackendByDomainH :: Domain ::: JsonRequest CustomBackend -> Galley Response
internalPutCustomBackendByDomainH (domain ::: req) = do
  customBackend <- fromJsonBody req
  -- simple enough to not need a separate function
  Data.setCustomBackend domain customBackend
  pure (empty & setStatus status201)

internalDeleteCustomBackendByDomainH :: Domain ::: JSON -> Galley Response
internalDeleteCustomBackendByDomainH (domain ::: _) = do
  Data.deleteCustomBackend domain
  pure (empty & setStatus status200)
