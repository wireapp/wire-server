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

module Federator.Error
  ( internalError,
    internalErrorWithDescription,
    remoteBackendNotFound,
  )
where

import Data.Domain (Domain, domainText)
import Data.String.Conversions (cs)
import Imports
import Network.HTTP.Types (status500)
import Network.Wai.Utilities.Error (Error (Error))

internalError :: Error
internalError = internalErrorWithDescription "internal error"

internalErrorWithDescription :: LText -> Error
internalErrorWithDescription = Error status500 "internal-error"

remoteBackendNotFound :: Domain -> Error
remoteBackendNotFound domain =
  Error
    status500
    "federation-backend-not-found"
    ("No SRV record for the domain " <> cs (domainText domain))
