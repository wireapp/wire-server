module Brig.Types.Servant.API.Internal (api, API(..)) where

import Brig.Types.Activation
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Servant.Orphans
import Brig.Types.User
import Data.ByteString.Conversion (List(..))
import Data.Id
import Galley.Types
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')
import Servant.API.Generic


api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

data API route = API
  { _getStatus :: route :- "status"     :> Get NoContent
  , _headStatus :: route :- "status"     :> Head NoContent
  , _getMonitoring :: route :- "monitoring" :> Get Metrics.Metrics
  , _postUsersConn :: route :- "users" :> Capture "uid" UserId :> "auto-connect"
      :> InternalZConn
      :> ReqBody UserSet
      :> Post [UserConnection]
     -- handler: autoConnect

  , _postUsers :: route :- "users"
      :> ReqBody NewUser
      :> Post (Headers '[Servant.Header "Location" UserId] SelfProfile)
     -- handler: createUserNoVerify

  , _putSelfEmail :: route :- "self" :> "email"
      :> InternalZUser
      :> ReqBody Brig.Types.User.EmailUpdate
      :> Put204 NoContent
     -- handler: changeSelfEmailNoSend

  , _deleteUsers :: route :- "users" :> Capture "uid" UserId
      :> Delete202 NoContent
     -- handler: deleteUserNoVerify

  , _GetUsersConnStatus :: route :- "users" :> "connections-status"
      :> QueryParamOptional "filter" Relation
      :> QueryParamStrict "users" UserId
      :> Get [ConnectionStatus]
     -- handler: deprecatedGetConnectionsStatus

  , _postUsersConnStatus :: route :- "users" :> "connections-status"
      :> QueryParamOptional "filter" Relation
      :> ReqBody ConnectionsStatusRequest
      :> Post [ConnectionStatus]
     -- handler: getConnectionsStatus

  , _getUsersByIds :: route :- "users"
      :> QueryParamStrict "ids" (List UserId)
      :> Get [UserAccount]
     -- handler: listActivatedAccounts

  , _getUsersByHandles :: route :- "users"
      :> QueryParamStrict "handles" (List Handle)
      :> Get [UserAccount]
     -- handler: listActivatedAccounts

  , _getUsersByEmail :: route :- "users"
      :> QueryParamStrict "email" Email
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity

  , _getUsersByPhone :: route :- "users"
      :> QueryParamStrict "phone" Phone
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity

  , _putUsersAccountStatus :: route :- "users" :> Capture "uid" UserId :> "status"
      :> ReqBody AccountStatusUpdate
      :> Put200 NoContent
     -- handler: changeAccountStatus

  , _getUsersAccountStatus :: route :- "users" :> Capture "uid" UserId :> "status"
      :> Get AccountStatusObject
     -- handler: getAccountStatus

  , _getUsersContacts :: route :- "users" :> Capture "uid" UserId :> "contacts"
      :> Get UserIds
     -- handler: getContactList

  , _getUsers :: route :- "users" :> "activation-code"
      :> QueryParamStrict "email" Email
      :> Get ActivationCodeObject
     -- handler: getActivationCode

  , _getActivationCode :: route :- "users" :> "activation-code"
      :> QueryParamStrict "phone" Phone
      :> Get ActivationCodeObject
     -- handler: getActivationCode

  , _getPasswordResetCodeEmail :: route :- "users" :> "password-reset-code"
      :> QueryParamStrict "email" Email
      :> Get ActivationCodeObject
     -- handler: getPasswordResetCode

  , _getPasswordResetCodePhone :: route :- "users" :> "password-reset-code"
      :> QueryParamStrict "phone" Phone
      :> Get ActivationCodeObject
     -- handler: getPasswordResetCode

  , _getUsersRevokeIdEmail :: route :- "users" :> "revoke-identity"
      :> QueryParamStrict "email" Email
      :> Get NoContent
     -- handler: revokeIdentity

  , _getUsersRevokeIdPhone :: route :- "users" :> "revoke-identity"
      :> QueryParamStrict "phone" Phone
      :> Get NoContent
     -- handler: revokeIdentity

  , _getUsersBlacklistEmail :: route :- "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Get NoContent
     -- handler: checkBlacklist

  , _getUsersBlacklistPhone :: route :- "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Get NoContent
     -- handler: checkBlacklist

  , _deleteUsersBlacklistEmail :: route :- "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Delete200 NoContent
     -- handler: deleteFromBlacklist

  , _deleteUsersBlacklistPhone :: route :- "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Delete200 NoContent
     -- handler: deleteFromBlacklist

  , _postUsersBlacklistEmail :: route :- "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Post NoContent
     -- handler: addBlacklist

  , _postUsersBlacklistPhone :: route :- "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Post NoContent
     -- handler: addBlacklist

    -- given a phone number (or phone number prefix), see whether
    -- it is blocked via a prefix (and if so, via which specific prefix)
  , _getUsersPhonePrefixes :: route :- "users" :> "phone-prefixes" :> Capture "prefix" PhonePrefix
      :> Get [ExcludedPrefix]
     -- handler: getPhonePrefixes

  , _deleteUsersPhonePrefixes :: route :- "users" :> "phone-prefixes" :> Capture "prefix" PhonePrefix
      :> Delete200 NoContent
     -- handler: deleteFromPhonePrefix

  , _postUsersPhonePrefixes :: route :- "users" :> "phone-prefixes"
      :> ReqBody ExcludedPrefix
      :> Post NoContent
     -- handler: addPhonePrefix

    -- is :uid not team owner, or there are other team owners?
  , _getUsersCanBeDeleted :: route :- "users" :> Capture "uid" UserId :> "can-be-deleted" :> Capture "tid" TeamId
      :> Get NoContent  -- info is encoded in dynamic response status code
     -- handler: canBeDeleted

    -- is :uid team owner (the only one or one of several)?
  , _getUsersIsTeamOwner :: route :- "users" :> Capture "uid" UserId :> "is-team-owner" :> Capture "tid" TeamId
      :> Get NoContent  -- info is encoded in dynamic response status code
     -- handler: isTeamOwner

  , _putUsersSsoId :: route :- "users" :> Capture "uid" UserId :> "sso-id"
      :> ReqBody UserSSOId
      :> Put200 NoContent
     -- handler: updateSSOId

  , _putUsersManagedBy :: route :- "users" :> Capture "uid" UserId :> "managed-by"
      :> ReqBody ManagedByUpdate
      :> Put200 NoContent
     -- handler: updateManagedBy

  , _putUsersRichInfo :: route :- "users" :> Capture "uid" UserId :> "rich-info"
      :> ReqBody RichInfoUpdate
      :> Put200 NoContent
     -- handler: updateRichInfo

  , _putUsersClients :: route :- "clients"
      :> ReqBody UserSet
      :> Post UserClients
     -- handler: internalListClients
  }
  deriving (Generic)
