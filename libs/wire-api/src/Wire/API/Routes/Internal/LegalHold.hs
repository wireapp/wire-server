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

module Wire.API.Routes.Internal.LegalHold where

import Control.Lens
import Data.Id
import Data.OpenApi (OpenApi)
import Data.OpenApi.Lens
import Data.Proxy
import Imports
import Servant.API
import Servant.OpenApi
import Wire.API.Team.Feature

type InternalLegalHoldAPI =
  "i"
    :> "teams"
    :> ( Capture "tid" TeamId
           :> "legalhold"
           :> Get '[JSON] (LockableFeature LegalholdConfig)
           :<|> Capture "tid" TeamId
             :> "legalhold"
             :> ReqBody '[JSON] (Feature LegalholdConfig)
             :> Put '[] NoContent
       )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @InternalLegalHoldAPI)
    & info . title .~ "Wire-Server internal legalhold API"
