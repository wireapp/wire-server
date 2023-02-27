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
    UserAPI,
    AuthAPI,
    EJPDRequest,
    GetAccountConferenceCallingConfig,
    PutAccountConferenceCallingConfig,
    DeleteAccountConferenceCallingConfig,
    swaggerDoc,
    module Wire.API.Routes.Internal.Brig.EJPD,
    NewKeyPackageRef (..),
    NewKeyPackage (..),
    NewKeyPackageResult (..),
  )
where

import Control.Lens ((.~))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Code as Code
import Data.Id as Id
import Data.Qualified (Qualified)
import Data.Schema hiding (swaggerDoc)
import Data.Swagger (HasInfo (info), HasTitle (title), Swagger)
import qualified Data.Swagger as S
import Imports hiding (head)
import Servant hiding (Handler, WithStatus, addHeader, respond)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Connection
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MakesFederatedCall
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Brig.EJPD
import Wire.API.Routes.Internal.Brig.OAuth (OAuthAPI)
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Team.Feature
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Auth.Sso
import Wire.API.User.Client

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

type GetAccountConferenceCallingConfig =
  Summary
    "Read cassandra field 'brig.user.feature_conference_calling'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Get '[Servant.JSON] (WithStatusNoLock ConferenceCallingConfig)

type PutAccountConferenceCallingConfig =
  Summary
    "Write to cassandra field 'brig.user.feature_conference_calling'"
    :> "users"
    :> Capture "uid" UserId
    :> "features"
    :> "conferenceCalling"
    :> Servant.ReqBody '[Servant.JSON] (WithStatusNoLock ConferenceCallingConfig)
    :> Put '[Servant.JSON] NoContent

type DeleteAccountConferenceCallingConfig =
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
      :<|> Named "get-account-conference-calling-config" GetAccountConferenceCallingConfig
      :<|> PutAccountConferenceCallingConfig
      :<|> DeleteAccountConferenceCallingConfig
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
        :> MakesFederatedCall 'Brig "on-user-deleted-connections"
        :> ReqBody '[Servant.JSON] NewUser
        :> MultiVerb 'POST '[Servant.JSON] RegisterInternalResponses (Either RegisterError SelfProfile)
    )
    :<|> Named
           "createUserNoVerifySpar"
           ( "users"
               :> "spar"
               :> MakesFederatedCall 'Brig "on-user-deleted-connections"
               :> ReqBody '[Servant.JSON] NewUserSpar
               :> MultiVerb 'POST '[Servant.JSON] CreateUserSparInternalResponses (Either CreateUserSparError SelfProfile)
           )

-- | The missing ref is implicit by the capture
data NewKeyPackageRef = NewKeyPackageRef
  { nkprUserId :: Qualified UserId,
    nkprClientId :: ClientId,
    nkprConversation :: Qualified ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewKeyPackageRef)

instance ToSchema NewKeyPackageRef where
  schema =
    object "NewKeyPackageRef" $
      NewKeyPackageRef
        <$> nkprUserId .= field "user_id" schema
        <*> nkprClientId .= field "client_id" schema
        <*> nkprConversation .= field "conversation" schema

data NewKeyPackage = NewKeyPackage
  { nkpConversation :: Qualified ConvId,
    nkpKeyPackage :: KeyPackageData
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewKeyPackage)

instance ToSchema NewKeyPackage where
  schema =
    object "NewKeyPackage" $
      NewKeyPackage
        <$> nkpConversation .= field "conversation" schema
        <*> nkpKeyPackage .= field "key_package" schema

data NewKeyPackageResult = NewKeyPackageResult
  { nkpresClientIdentity :: ClientIdentity,
    nkpresKeyPackageRef :: KeyPackageRef
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewKeyPackageResult)

instance ToSchema NewKeyPackageResult where
  schema =
    object "NewKeyPackageResult" $
      NewKeyPackageResult
        <$> nkpresClientIdentity .= field "client_identity" schema
        <*> nkpresKeyPackageRef .= field "key_package_ref" schema

type MLSAPI =
  "mls"
    :> ( ( "key-packages"
             :> Capture "ref" KeyPackageRef
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
                    :<|> Named
                           "put-key-package-ref"
                           ( Summary "Create a new KeyPackageRef mapping"
                               :> ReqBody '[Servant.JSON] NewKeyPackageRef
                               :> MultiVerb
                                    'PUT
                                    '[Servant.JSON]
                                    '[RespondEmpty 201 "Key package ref mapping created"]
                                    ()
                           )
                    :<|> Named
                           "post-key-package-ref"
                           ( Summary "Update a KeyPackageRef in mapping"
                               :> ReqBody '[Servant.JSON] KeyPackageRef
                               :> MultiVerb
                                    'POST
                                    '[Servant.JSON]
                                    '[RespondEmpty 201 "Key package ref mapping updated"]
                                    ()
                           )
                )
         )
           :<|> GetMLSClients
           :<|> MapKeyPackageRefs
           :<|> Named
                  "put-key-package-add"
                  ( "key-package-add"
                      :> ReqBody '[Servant.JSON] NewKeyPackage
                      :> MultiVerb1
                           'PUT
                           '[Servant.JSON]
                           (Respond 200 "Key package ref mapping updated" NewKeyPackageResult)
                  )
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
  Summary "Return all clients and all MLS-capable clients of a user"
    :> "clients"
    :> CanThrow 'UserNotFound
    :> Capture "user" UserId
    :> QueryParam' '[Required, Strict] "sig_scheme" SignatureSchemeTag
    :> MultiVerb1
         'GET
         '[Servant.JSON]
         (Respond 200 "MLS clients" (Set ClientInfo))

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
    :> ( EJPD_API
           :<|> AccountAPI
           :<|> MLSAPI
           :<|> GetVerificationCode
           :<|> TeamsAPI
           :<|> UserAPI
           :<|> AuthAPI
           :<|> OAuthAPI
       )

type TeamsAPI =
  Named
    "updateSearchVisibilityInbound"
    ( "teams"
        :> ReqBody '[Servant.JSON] (Multi.TeamStatus SearchVisibilityInboundConfig)
        :> Post '[Servant.JSON] ()
    )

type UserAPI =
  UpdateUserLocale
    :<|> DeleteUserLocale
    :<|> GetDefaultLocale

type UpdateUserLocale =
  Summary
    "Set the user's locale"
    :> "users"
    :> Capture "uid" UserId
    :> "locale"
    :> ReqBody '[Servant.JSON] LocaleUpdate
    :> Put '[Servant.JSON] LocaleUpdate

type DeleteUserLocale =
  Summary
    "Delete the user's locale"
    :> "users"
    :> Capture "uid" UserId
    :> "locale"
    :> Delete '[Servant.JSON] NoContent

type GetDefaultLocale =
  Summary "Get the default locale"
    :> "users"
    :> "locale"
    :> Get '[Servant.JSON] LocaleUpdate

type AuthAPI =
  Named
    "legalhold-login"
    ( "legalhold-login"
        :> MakesFederatedCall 'Brig "on-user-deleted-connections"
        :> ReqBody '[JSON] LegalHoldLogin
        :> MultiVerb1 'POST '[JSON] TokenResponse
    )
    :<|> Named
           "sso-login"
           ( "sso-login"
               :> MakesFederatedCall 'Brig "on-user-deleted-connections"
               :> ReqBody '[JSON] SsoLogin
               :> QueryParam' [Optional, Strict] "persist" Bool
               :> MultiVerb1 'POST '[JSON] TokenResponse
           )
    :<|> Named
           "login-code"
           ( "users"
               :> "login-code"
               :> QueryParam' [Required, Strict] "phone" Phone
               :> MultiVerb1 'GET '[JSON] (Respond 200 "Login code" PendingLoginCode)
           )
    :<|> Named
           "reauthenticate"
           ( "users"
               :> Capture "uid" UserId
               :> "reauthenticate"
               :> ReqBody '[JSON] ReAuthUser
               :> MultiVerb1 'GET '[JSON] (RespondEmpty 200 "OK")
           )

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @API)
    & info . title .~ "Wire-Server internal brig API"
