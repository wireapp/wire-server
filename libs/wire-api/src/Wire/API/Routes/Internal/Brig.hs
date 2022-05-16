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

module Wire.API.Routes.Internal.Brig
  ( API,
    EJPD_API,
    AccountAPI,
    MLSAPI,
    TeamsAPI,
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
import qualified Data.Code as Code
import Data.Id as Id
import Data.Qualified (Qualified)
import Data.Swagger (HasInfo (info), HasTitle (title), Swagger)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, respond)
import qualified Servant
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Connection
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Brig.EJPD
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Team.Feature (TeamFeatureName (TeamFeatureSearchVisibilityInbound))
import qualified Wire.API.Team.Feature as ApiFt
import Wire.API.User

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
    :> Get '[Servant.JSON] (ApiFt.TeamFeatureStatus 'ApiFt.WithoutLockStatus 'ApiFt.TeamFeatureConferenceCalling)

type PutAccountFeatureConfig =
  Summary
    "Write to cassandra field 'brig.user.feature_conference_calling'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Servant.ReqBody '[Servant.JSON] (ApiFt.TeamFeatureStatus 'ApiFt.WithoutLockStatus 'ApiFt.TeamFeatureConferenceCalling)
    :> Put '[Servant.JSON] NoContent

type DeleteAccountFeatureConfig =
  Summary
    "Reset cassandra field 'brig.user.feature_conference_calling' to 'null'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Delete '[Servant.JSON] NoContent

type GetAllConnectionsUnqualified =
  Summary "Get all connections of a given user"
    :> "users"
    :> "connections-status"
    :> ReqBody '[Servant.JSON] ConnectionsStatusRequest
    :> QueryParam'
         [ Optional,
           Strict,
           Description "Only returns connections with the given relation, if omitted, returns all connections"
         ]
         "filter"
         Relation
    :> Post '[Servant.JSON] [ConnectionStatus]

type GetAllConnections =
  Summary "Get all connections of a given user"
    :> "users"
    :> "connections-status"
    :> "v2"
    :> ReqBody '[Servant.JSON] ConnectionsStatusRequestV2
    :> Post '[Servant.JSON] [ConnectionStatusV2]

type EJPD_API =
  ( EJPDRequest
      :<|> Named "get-account-feature-config" GetAccountFeatureConfig
      :<|> PutAccountFeatureConfig
      :<|> DeleteAccountFeatureConfig
      :<|> GetAllConnectionsUnqualified
      :<|> GetAllConnections
  )

type AccountAPI =
  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to created user, if it is a team invitation or user has an SSO ID
  -- - UserIdentityUpdated event to created user, if email or phone get activated
  Named
    "createUserNoVerify"
    ( "users"
        :> ReqBody '[Servant.JSON] NewUser
        :> MultiVerb 'POST '[Servant.JSON] RegisterInternalResponses (Either RegisterError SelfProfile)
    )

type MLSAPI =
  "mls"
    :> ( ( "key-packages" :> Capture "ref" KeyPackageRef
             :> ( Named
                    "get-client-by-key-package-ref"
                    ( Summary "Resolve an MLS key package ref to a qualified client ID"
                        :> MultiVerb
                             'GET
                             '[Servant.JSON]
                             '[ RespondEmpty 404 "Key package ref not found",
                                Respond 200 "Key package ref found" ClientIdentity
                              ]
                             (Maybe ClientIdentity)
                    )
                    :<|> ( "conversation"
                             :> ( PutConversationByKeyPackageRef
                                    :<|> GetConversationByKeyPackageRef
                                )
                         )
                )
         )
           :<|> GetMLSClients
           :<|> MapKeyPackageRefs
       )

type PutConversationByKeyPackageRef =
  Named
    "put-conversation-by-key-package-ref"
    ( Summary "Associate a conversation with a key package"
        :> ReqBody '[Servant.JSON] (Qualified ConvId)
        :> MultiVerb
             'PUT
             '[Servant.JSON]
             [ RespondEmpty 404 "No key package found by reference",
               RespondEmpty 204 "Converstaion associated"
             ]
             Bool
    )

type GetConversationByKeyPackageRef =
  Named
    "get-conversation-by-key-package-ref"
    ( Summary
        "Retrieve the conversation associated with a key package"
        :> MultiVerb
             'GET
             '[Servant.JSON]
             [ RespondEmpty 404 "No associated conversation or bad key package",
               Respond 200 "Conversation found" (Qualified ConvId)
             ]
             (Maybe (Qualified ConvId))
    )

type GetMLSClients =
  Summary "Return all MLS-enabled clients of a user"
    :> "clients"
    :> CanThrow 'UserNotFound
    :> Capture "user" UserId
    :> QueryParam' '[Required, Strict] "sig_scheme" SignatureSchemeTag
    :> MultiVerb1
         'GET
         '[Servant.JSON]
         (Respond 200 "MLS clients" (Set ClientId))

type MapKeyPackageRefs =
  Summary "Insert bundle into the KeyPackage ref mapping. Only for tests."
    :> "key-package-refs"
    :> ReqBody '[Servant.JSON] KeyPackageBundle
    :> MultiVerb 'PUT '[Servant.JSON] '[RespondEmpty 204 "Mapping was updated"] ()

type GetVerificationCode =
  Summary "Get verification code for a given email and action"
    :> "users"
    :> Capture "uid" UserId
    :> "verification-code"
    :> Capture "action" VerificationAction
    :> Get '[Servant.JSON] (Maybe Code.Value)

type API =
  "i"
    :> (EJPD_API :<|> AccountAPI :<|> MLSAPI :<|> GetVerificationCode :<|> TeamsAPI)

type TeamsAPI =
  Named
    "updateSearchVisibilityInbound"
    ( "teams"
        :> ReqBody '[Servant.JSON] (Multi.TeamStatusUpdate 'TeamFeatureSearchVisibilityInbound)
        :> Post '[Servant.JSON] ()
    )

type SwaggerDocsAPI = "api" :> "internal" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @API)
    & info . title .~ "Wire-Server API as Swagger 2.0 (internal end-points; incomplete) "
