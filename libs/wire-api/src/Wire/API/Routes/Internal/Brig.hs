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

module Wire.API.Routes.Internal.Brig
  ( API,
    EJPDRequest,
    GetAccountFeatureConfig,
    PutAccountFeatureConfig,
    DeleteAccountFeatureConfig,
    SwaggerDocsAPI,
    swaggerDoc,
    module Wire.API.Routes.Internal.Brig.EJPD,
  )
where

import Control.Lens ((.~))
import Data.Id as Id
import Data.Swagger (HasInfo (info), HasTitle (title), Swagger)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, respond)
import qualified Servant
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Routes.Internal.Brig.EJPD
import qualified Wire.API.Team.Feature as ApiFt

type EJPDRequest =
  Summary
    "Identify users for law enforcement.  Wire has legal requirements to cooperate \
    \with the authorities.  The wire backend operations team uses this to answer \
    \identification requests manually.  It is our best-effort representation of the \
    \minimum required information we need to hand over about targets and (in some \
    \cases) their communication peers.  For more information, consult ejpd.admin.ch."
    :> "ejpd-request"
    :> QueryParam'
         [ Optional,
           Strict,
           Description "Also provide information about all contacts of the identified users"
         ]
         "include_contacts"
         Bool
    :> Servant.ReqBody '[Servant.JSON] EJPDRequestBody
    :> Post '[Servant.JSON] EJPDResponseBody

type GetAccountFeatureConfig =
  Summary
    "Read cassandra field 'brig.user.feature_conference_calling'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Get '[Servant.JSON] (ApiFt.TeamFeatureStatus 'ApiFt.TeamFeatureConferenceCalling)

type PutAccountFeatureConfig =
  Summary
    "Write to cassandra field 'brig.user.feature_conference_calling'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Servant.ReqBody '[Servant.JSON] (ApiFt.TeamFeatureStatus 'ApiFt.TeamFeatureConferenceCalling)
    :> Put '[Servant.JSON] NoContent

type DeleteAccountFeatureConfig =
  Summary
    "Reset cassandra field 'brig.user.feature_conference_calling' to 'null'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Delete '[Servant.JSON] NoContent

type API =
  "i"
    :> ( EJPDRequest
           :<|> GetAccountFeatureConfig
           :<|> PutAccountFeatureConfig
           :<|> DeleteAccountFeatureConfig
       )

type SwaggerDocsAPI = "api" :> "internal" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @API)
    & info . title .~ "Wire-Server API as Swagger 2.0 (internal end-points; incomplete) "
