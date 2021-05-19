-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.LegalHold where

import Data.Id
import Data.Proxy
import Data.Swagger hiding (Header (..))
import Servant.API hiding (Header)
import Servant.Swagger
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold

type ServantAPI = PublicAPI :<|> InternalAPI

-- FUTUREWORK: restructure this for readability and add missing bodies
type PublicAPI =
  "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
    :> ReqBody '[JSON] NewLegalHoldService
    :> Post '[JSON] ViewLegalHoldService
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
      :> Get '[JSON] ViewLegalHoldService
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
      -- :> ReqBody '[JSON] RemoveLegalHoldSettingsRequest
      :> Verb 'DELETE 204 '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "consent"
      :> Post '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      :> Post '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId :> "approve"
      -- :> ReqBody '[JSON] ApproveLegalHoldForUserRequest
      :> Verb 'PUT 204 '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      :> Get '[JSON] UserLegalHoldStatusResponse
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      -- :> ReqBody '[JSON] DisableLegalHoldForUserRequest
      :> Verb 'DELETE 204 '[] NoContent

type InternalAPI =
  "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
    :> Get '[JSON] (TeamFeatureStatus 'TeamFeatureLegalHold)
    :<|> "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
      :> ReqBody '[JSON] (TeamFeatureStatus 'TeamFeatureLegalHold)
      :> Put '[] NoContent

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
