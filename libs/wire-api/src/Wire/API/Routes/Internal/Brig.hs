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
    BrigInternalClient,
    brigInternalClient,
    runBrigInternalClient,
    IStatusAPI,
    EJPD_API,
    AccountAPI,
    MLSAPI,
    TeamsAPI,
    UserAPI,
    ClientAPI,
    AuthAPI,
    FederationRemotesAPI,
    EJPDRequest,
    ISearchIndexAPI,
    GetAccountConferenceCallingConfig,
    PutAccountConferenceCallingConfig,
    DeleteAccountConferenceCallingConfig,
    swaggerDoc,
    module Wire.API.Routes.Internal.Brig.EJPD,
    FoundInvitationCode (..),
  )
where

import Control.Lens ((.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Code qualified as Code
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Handle (Handle)
import Data.Id as Id
import Data.OpenApi (HasInfo (info), HasTitle (title), OpenApi)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified)
import Data.Schema hiding (swaggerDoc)
import Data.Text qualified as Text
import GHC.TypeLits
import Imports hiding (head)
import Network.HTTP.Client qualified as HTTP
import Servant hiding (Handler, WithStatus, addHeader, respond)
import Servant.Client qualified as Servant
import Servant.Client.Core qualified as Servant
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.OpenApi.Internal.Orphans ()
import Util.Options
import Wire.API.Connection
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.MLS.CipherSuite
import Wire.API.MakesFederatedCall
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Brig.EJPD
import Wire.API.Routes.Internal.Brig.OAuth (OAuthAPI)
import Wire.API.Routes.Internal.Brig.SearchIndex (ISearchIndexAPI)
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public (ZUser)
import Wire.API.Team.Feature
import Wire.API.Team.Invitation (Invitation)
import Wire.API.Team.LegalHold.Internal
import Wire.API.Team.Size qualified as Teamsize
import Wire.API.User hiding (InvitationCode)
import Wire.API.User qualified as User
import Wire.API.User.Auth
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Auth.Sso
import Wire.API.User.Client
import Wire.API.User.RichInfo

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
        :> MakesFederatedCall 'Brig "send-connection-action"
        :> ReqBody '[Servant.JSON] NewUser
        :> MultiVerb 'POST '[Servant.JSON] RegisterInternalResponses (Either RegisterError SelfProfile)
    )
    :<|> Named
           "createUserNoVerifySpar"
           ( "users"
               :> "spar"
               :> MakesFederatedCall 'Brig "on-user-deleted-connections"
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> ReqBody '[Servant.JSON] NewUserSpar
               :> MultiVerb 'POST '[Servant.JSON] CreateUserSparInternalResponses (Either CreateUserSparError SelfProfile)
           )
    :<|> Named
           "putSelfEmail"
           ( Summary
               "internal email activation (used in tests and in spar for validating emails obtained as \
               \SAML user identifiers).  if the validate query parameter is false or missing, only set \
               \the activation timeout, but do not send an email, and do not do anything about \
               \activating the email."
               :> ZUser
               :> "self"
               :> "email"
               :> ReqBody '[Servant.JSON] EmailUpdate
               :> QueryParam' [Optional, Strict, Description "whether to send validation email, or activate"] "validate" Bool
               :> MultiVerb
                    'PUT
                    '[Servant.JSON]
                    '[ Respond 202 "Update accepted and pending activation of the new email" (),
                       Respond 204 "No update, current and new email address are the same" ()
                     ]
                    ChangeEmailResponse
           )
    :<|> Named
           "iDeleteUser"
           ( Summary
               "This endpoint will lead to the following events being sent: UserDeleted event to all of \
               \its contacts, MemberLeave event to members for all conversations the user was in (via galley)"
               :> CanThrow 'UserNotFound
               :> "users"
               :> Capture "uid" UserId
               :> MultiVerb
                    'DELETE
                    '[Servant.JSON]
                    '[ Respond 200 "UserResponseAccountAlreadyDeleted" (),
                       Respond 202 "UserResponseAccountDeleted" ()
                     ]
                    DeleteUserResponse
           )
    :<|> Named
           "iPutUserStatus"
           ( -- FUTUREWORK: `CanThrow ... :>`
             "users"
               :> Capture "uid" UserId
               :> "status"
               :> ReqBody '[Servant.JSON] AccountStatusUpdate
               :> Put '[Servant.JSON] NoContent
           )
    :<|> Named
           "iGetUserStatus"
           ( CanThrow 'UserNotFound
               :> "users"
               :> Capture "uid" UserId
               :> "status"
               :> Get '[Servant.JSON] AccountStatusResp
           )
    :<|> Named
           "iGetUsersByVariousKeys"
           ( "users"
               :> QueryParam' [Optional, Strict] "ids" (CommaSeparatedList UserId)
               :> QueryParam' [Optional, Strict] "handles" (CommaSeparatedList Handle)
               :> QueryParam' [Optional, Strict] "email" (CommaSeparatedList Email) -- don't rename to `emails`, for backwards compat!
               :> QueryParam' [Optional, Strict] "phone" (CommaSeparatedList Phone) -- don't rename to `phones`, for backwards compat!
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Also return new accounts with team invitation pending"
                    ]
                    "includePendingInvitations"
                    Bool
               :> Get '[Servant.JSON] [UserAccount]
           )
    :<|> Named
           "iGetUserContacts"
           ( "users"
               :> Capture "uid" UserId
               :> "contacts"
               :> Get '[Servant.JSON] UserIds
           )
    :<|> Named
           "iGetUserActivationCode"
           ( "users"
               :> "activation-code"
               :> QueryParam' [Optional, Strict] "email" Email
               :> QueryParam' [Optional, Strict] "phone" Phone
               :> Get '[Servant.JSON] GetActivationCodeResp
           )
    :<|> Named
           "iGetUserPasswordResetCode"
           ( "users"
               :> "password-reset-code"
               :> QueryParam' [Optional, Strict] "email" Email
               :> QueryParam' [Optional, Strict] "phone" Phone
               :> Get '[Servant.JSON] GetPasswordResetCodeResp
           )
    :<|> Named
           "iRevokeIdentity"
           ( Summary "This endpoint can lead to the following events being sent: UserIdentityRemoved event to target user"
               :> "users"
               :> "revoke-identity"
               :> QueryParam' [Optional, Strict] "email" Email
               :> QueryParam' [Optional, Strict] "phone" Phone
               :> Post '[Servant.JSON] NoContent
           )
    :<|> Named
           "iHeadBlacklist"
           ( "users"
               :> "blacklist"
               :> QueryParam' [Optional, Strict] "email" Email
               :> QueryParam' [Optional, Strict] "phone" Phone
               :> MultiVerb
                    'GET
                    '[Servant.JSON]
                    '[ Respond 404 "Not blacklisted" (),
                       Respond 200 "Yes blacklisted" ()
                     ]
                    CheckBlacklistResponse
           )
    :<|> Named
           "iDeleteBlacklist"
           ( "users"
               :> "blacklist"
               :> QueryParam' [Optional, Strict] "email" Email
               :> QueryParam' [Optional, Strict] "phone" Phone
               :> Delete '[Servant.JSON] NoContent
           )
    :<|> Named
           "iPostBlacklist"
           ( "users"
               :> "blacklist"
               :> QueryParam' [Optional, Strict] "email" Email
               :> QueryParam' [Optional, Strict] "phone" Phone
               :> Post '[Servant.JSON] NoContent
           )
    :<|> Named
           "iGetPhonePrefix"
           ( Summary
               "given a phone number (or phone number prefix), see whether it is blocked \
               \via a prefix (and if so, via which specific prefix)"
               :> "users"
               :> "phone-prefixes"
               :> Capture "prefix" PhonePrefix
               :> MultiVerb
                    'GET
                    '[Servant.JSON]
                    '[ RespondEmpty 404 "PhonePrefixNotFound",
                       Respond 200 "PhonePrefixesFound" [ExcludedPrefix]
                     ]
                    GetPhonePrefixResponse
           )
    :<|> Named
           "iDeletePhonePrefix"
           ( "users"
               :> "phone-prefixes"
               :> Capture "prefix" PhonePrefix
               :> Delete '[Servant.JSON] NoContent
           )
    :<|> Named
           "iPostPhonePrefix"
           ( "users"
               :> "phone-prefixes"
               :> ReqBody '[Servant.JSON] ExcludedPrefix
               :> Post '[Servant.JSON] NoContent
           )
    :<|> Named
           "iPutUserSsoId"
           ( "users"
               :> Capture "uid" UserId
               :> "sso-id"
               :> ReqBody '[Servant.JSON] UserSSOId
               :> MultiVerb
                    'PUT
                    '[Servant.JSON]
                    '[ RespondEmpty 200 "UpdateSSOIdSuccess",
                       RespondEmpty 404 "UpdateSSOIdNotFound"
                     ]
                    UpdateSSOIdResponse
           )
    :<|> Named
           "iDeleteUserSsoId"
           ( "users"
               :> Capture "uid" UserId
               :> "sso-id"
               :> MultiVerb
                    'DELETE
                    '[Servant.JSON]
                    '[ RespondEmpty 200 "UpdateSSOIdSuccess",
                       RespondEmpty 404 "UpdateSSOIdNotFound"
                     ]
                    UpdateSSOIdResponse
           )
    :<|> Named
           "iPutManagedBy"
           ( "users"
               :> Capture "uid" UserId
               :> "managed-by"
               :> ReqBody '[Servant.JSON] ManagedByUpdate
               :> Put '[Servant.JSON] NoContent
           )
    :<|> Named
           "iPutRichInfo"
           ( "users"
               :> Capture "uid" UserId
               :> "rich-info"
               :> ReqBody '[Servant.JSON] RichInfoUpdate
               :> Put '[Servant.JSON] NoContent
           )
    :<|> Named
           "iPutHandle"
           ( "users"
               :> Capture "uid" UserId
               :> "handle"
               :> ReqBody '[Servant.JSON] HandleUpdate
               :> Put '[Servant.JSON] NoContent
           )
    :<|> Named
           "iPutHandle"
           ( "users"
               :> Capture "uid" UserId
               :> "name"
               :> ReqBody '[Servant.JSON] NameUpdate
               :> Put '[Servant.JSON] NoContent
           )
    :<|> Named
           "iGetRichInfo"
           ( "users"
               :> Capture "uid" UserId
               :> "rich-info"
               :> Get '[Servant.JSON] RichInfo
           )
    :<|> Named
           "iGetRichInfoMulti"
           ( "users"
               :> "rich-info"
               :> QueryParam' '[Optional, Strict] "ids" (CommaSeparatedList UserId)
               :> Get '[Servant.JSON] [(UserId, RichInfo)]
           )
    :<|> Named
           "iHeadHandle"
           ( CanThrow 'InvalidHandle
               :> "users"
               :> "handles"
               :> Capture "handle" Handle
               :> MultiVerb
                    'HEAD
                    '[Servant.JSON]
                    '[ RespondEmpty 200 "CheckHandleResponseFound",
                       RespondEmpty 404 "CheckHandleResponseNotFound"
                     ]
                    CheckHandleResponse
           )
    :<|> Named
           "iConnectionUpdate"
           ( "connections"
               :> "connection-update"
               :> ReqBody '[Servant.JSON] UpdateConnectionsInternal
               :> Put '[Servant.JSON] NoContent
           )
    :<|> Named
           "iListClients"
           ( "clients"
               :> ReqBody '[Servant.JSON] UserSet
               :> Post '[Servant.JSON] UserClients
           )
    :<|> Named
           "iListClientsFull"
           ( "clients"
               :> "full"
               :> ReqBody '[Servant.JSON] UserSet
               :> Post '[Servant.JSON] UserClientsFull
           )
    :<|> Named
           "iAddClient"
           ( Summary
               "This endpoint can lead to the following events being sent: ClientAdded event to the user; \
               \ClientRemoved event to the user, if removing old clients due to max number of clients; \
               \UserLegalHoldEnabled event to contacts of the user, if client type is legalhold."
               :> "clients"
               :> Capture "uid" UserId
               :> QueryParam' [Optional, Strict] "skip_reauth" Bool
               :> ReqBody '[Servant.JSON] NewClient
               :> Header' [Optional, Strict] "Z-Connection" ConnId
               :> Verb 'POST 201 '[Servant.JSON] Client
           )
    :<|> Named
           "iLegalholdAddClient"
           ( Summary
               "This endpoint can lead to the following events being sent: \
               \LegalHoldClientRequested event to contacts of the user"
               :> "clients"
               :> "legalhold"
               :> Capture "uid" UserId
               :> "request"
               :> ReqBody '[Servant.JSON] LegalHoldClientRequest
               :> Post '[Servant.JSON] NoContent
           )
    :<|> Named
           "iLegalholdDeleteClient"
           ( Summary
               "This endpoint can lead to the following events being sent: \
               \ClientRemoved event to the user; UserLegalHoldDisabled event \
               \to contacts of the user"
               :> "clients"
               :> "legalhold"
               :> Capture "uid" UserId
               :> Delete '[Servant.JSON] NoContent
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

type MLSAPI = "mls" :> GetMLSClients

type GetMLSClients =
  Summary "Return all clients and all MLS-capable clients of a user"
    :> "clients"
    :> CanThrow 'UserNotFound
    :> Capture "user" UserId
    :> QueryParam' '[Required, Strict] "ciphersuite" CipherSuite
    :> MultiVerb1
         'GET
         '[Servant.JSON]
         (Respond 200 "MLS clients" (Set ClientInfo))

type GetVerificationCode =
  Summary "Get verification code for a given email and action"
    :> "users"
    :> Capture "uid" UserId
    :> "verification-code"
    :> Capture "action" VerificationAction
    :> Get '[Servant.JSON] (Maybe Code.Value)

type API =
  "i"
    :> ( IStatusAPI
           :<|> EJPD_API
           :<|> AccountAPI
           :<|> MLSAPI
           :<|> GetVerificationCode
           :<|> TeamsAPI
           :<|> UserAPI
           :<|> ClientAPI
           :<|> AuthAPI
           :<|> OAuthAPI
           :<|> ISearchIndexAPI
           :<|> FederationRemotesAPI
       )

type IStatusAPI =
  Named
    "get-status"
    ( Summary "do nothing, just check liveness (NB: this works for both get, head)"
        :> "status"
        :> Get '[Servant.JSON] NoContent
    )

type TeamsAPI =
  Named
    "updateSearchVisibilityInbound"
    ( "teams"
        :> ReqBody '[Servant.JSON] (Multi.TeamStatus SearchVisibilityInboundConfig)
        :> Post '[Servant.JSON] ()
    )
    :<|> InvitationByEmail
    :<|> InvitationCode
    :<|> SuspendTeam
    :<|> UnsuspendTeam
    :<|> TeamSize
    :<|> TeamInvitations

type InvitationByEmail =
  Named
    "get-invitation-by-email"
    ( "teams"
        :> "invitations"
        :> "by-email"
        :> QueryParam' [Required, Strict] "email" Email
        :> Get '[Servant.JSON] Invitation
    )

type InvitationCode =
  Named
    "get-invitation-code"
    ( "teams"
        :> "invitation-code"
        :> QueryParam' [Required, Strict] "team" TeamId
        :> QueryParam' [Required, Strict] "invitation_id" InvitationId
        :> Get '[Servant.JSON] FoundInvitationCode
    )

newtype FoundInvitationCode = FoundInvitationCode {getFoundInvitationCode :: User.InvitationCode}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema FoundInvitationCode)

instance ToSchema FoundInvitationCode where
  schema =
    FoundInvitationCode
      <$> getFoundInvitationCode .= object "FoundInvitationCode" (field "code" (schema @User.InvitationCode))

type SuspendTeam =
  Named
    "suspend-team"
    ( "teams"
        :> Capture "tid" TeamId
        :> "suspend"
        :> Post
             '[Servant.JSON]
             NoContent
    )

type UnsuspendTeam =
  Named
    "unsuspend-team"
    ( "teams"
        :> Capture "tid" TeamId
        :> "unsuspend"
        :> Post
             '[Servant.JSON]
             NoContent
    )

type TeamSize =
  Named
    "team-size"
    ( "teams"
        :> Capture "tid" TeamId
        :> "size"
        :> Get '[JSON] Teamsize.TeamSize
    )

type TeamInvitations =
  Named
    "create-invitations-via-scim"
    ( "teams"
        :> Capture "tid" TeamId
        :> "invitations"
        :> Servant.ReqBody '[JSON] NewUserScimInvitation
        :> Post '[JSON] UserAccount
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

type ClientAPI =
  Named
    "update-client-last-active"
    ( Summary "Update last_active field of a client"
        :> "clients"
        :> Capture "uid" UserId
        :> Capture "client" ClientId
        :> "activity"
        :> MultiVerb1 'POST '[Servant.JSON] (RespondEmpty 200 "OK")
    )

type AuthAPI =
  Named
    "legalhold-login"
    ( "legalhold-login"
        :> MakesFederatedCall 'Brig "on-user-deleted-connections"
        :> MakesFederatedCall 'Brig "send-connection-action"
        :> ReqBody '[JSON] LegalHoldLogin
        :> MultiVerb1 'POST '[JSON] TokenResponse
    )
    :<|> Named
           "sso-login"
           ( "sso-login"
               :> MakesFederatedCall 'Brig "on-user-deleted-connections"
               :> MakesFederatedCall 'Brig "send-connection-action"
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

-- | This is located in brig, not in federator, because brig has a cassandra instance.  This
-- is not ideal, and other services could keep their local in-ram copy of this table up to date
-- via rabbitmq, but FUTUREWORK.
type FederationRemotesAPI =
  Named
    "add-federation-remotes"
    ( Description FederationRemotesAPIDescription
        :> "federation"
        :> "remotes"
        :> ReqBody '[JSON] FederationDomainConfig
        :> Post '[JSON] ()
    )
    :<|> Named
           "get-federation-remotes"
           ( Description FederationRemotesAPIDescription
               :> "federation"
               :> "remotes"
               :> Get '[JSON] FederationDomainConfigs
           )
    :<|> Named
           "update-federation-remotes"
           ( Description FederationRemotesAPIDescription
               :> "federation"
               :> "remotes"
               :> Capture "domain" Domain
               :> ReqBody '[JSON] FederationDomainConfig
               :> Put '[JSON] ()
           )
    :<|> Named
           "add-federation-remote-team"
           ( Description
               "Add a remote team to the list of teams that are allowed to federate with our domain"
               :> "federation"
               :> "remotes"
               :> Capture "domain" Domain
               :> "teams"
               :> ReqBody '[JSON] FederationRemoteTeam
               :> Post '[JSON] ()
           )
    :<|> Named
           "get-federation-remote-teams"
           ( Description
               "Get a list of teams from a remote domain that our backend is allowed to federate with."
               :> "federation"
               :> "remotes"
               :> Capture "domain" Domain
               :> "teams"
               :> Get '[JSON] [FederationRemoteTeam]
           )
    :<|> Named
           "delete-federation-remote-team"
           ( Description
               "Remove a remote team from the list of teams that are allowed to federate with our domain"
               :> "federation"
               :> "remotes"
               :> Capture "domain" Domain
               :> "teams"
               :> Capture "team" TeamId
               :> Delete '[JSON] ()
           )

type FederationRemotesAPIDescription =
  "See https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections for background. "

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @API)
    & info . title .~ "Wire-Server internal brig API"

newtype BrigInternalClient a = BrigInternalClient (Servant.ClientM a)
  deriving newtype (Functor, Applicative, Monad, Servant.RunClient)

brigInternalClient :: forall (name :: Symbol) endpoint. (HasEndpoint API endpoint name, Servant.HasClient BrigInternalClient endpoint) => Servant.Client BrigInternalClient endpoint
brigInternalClient = namedClient @API @name @BrigInternalClient

runBrigInternalClient :: HTTP.Manager -> Endpoint -> BrigInternalClient a -> IO (Either Servant.ClientError a)
runBrigInternalClient httpMgr (Endpoint brigHost brigPort) (BrigInternalClient action) = do
  let baseUrl = Servant.BaseUrl Servant.Http (Text.unpack brigHost) (fromIntegral brigPort) ""
      clientEnv = Servant.ClientEnv httpMgr baseUrl Nothing Servant.defaultMakeClientRequest
  Servant.runClientM action clientEnv
