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
import Data.Proxy
import Data.Swagger
import Imports
import Servant.API hiding (Header, WithStatus)
import Servant.Swagger
import Wire.API.SwaggerServant
import Wire.API.Team.Feature

type InternalLegalHoldAPI =
  SwaggerTag "legalhold"
    :> "i"
    :> "teams"
    :> ( Capture "tid" TeamId
           :> "legalhold"
           :> Get '[JSON] (WithStatus LegalholdConfig)
           :<|> Capture "tid" TeamId
             :> "legalhold"
             :> ReqBody '[JSON] (WithStatusNoLock LegalholdConfig)
             :> Put '[] NoContent
       )

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @InternalLegalHoldAPI)
    & info . title .~ "Wire-Server internal cargohold API"
