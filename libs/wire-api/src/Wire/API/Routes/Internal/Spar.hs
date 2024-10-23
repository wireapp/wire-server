-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Internal.Spar where

import Control.Lens
import Data.Id
import Data.OpenApi
import Imports
import Servant
import Servant.OpenApi
import Wire.API.User
import Wire.API.User.Saml

type InternalAPI =
  "i"
    :> ( Named "i_status" ("status" :> Get '[JSON] NoContent)
           :<|> Named "i_delete_team" ("teams" :> Capture "team" TeamId :> DeleteNoContent)
           :<|> Named "i_put_sso_settings" ("sso" :> "settings" :> ReqBody '[JSON] SsoSettings :> Put '[JSON] NoContent)
           :<|> Named "i_post_scim_user_info" ("scim" :> "userinfo" :> Capture "user" UserId :> Post '[JSON] ScimUserInfo)
       )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @InternalAPI)
    & info . title .~ "Wire-Server internal spar API"
