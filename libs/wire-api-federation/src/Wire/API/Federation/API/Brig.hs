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

module Wire.API.Federation.API.Brig where

import Control.Lens.Combinators hiding (element, enum)
import qualified Data.Aeson as A
import Data.Handle (Handle)
import Data.Id
import Data.Proxy
import Data.Range
import Data.Schema
import Data.Singletons (sing)
import qualified Data.Swagger as S
import Imports
import Servant.API
import Servant.API.Generic
import Servant.Swagger
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (..))
import Wire.API.Federation.API.Common
import Wire.API.Federation.Domain (OriginDomainHeader)
import Wire.API.Message (UserClients)
import Wire.API.User (UserProfile)
import Wire.API.User.Client (PubClient, UserClientPrekeyMap)
import Wire.API.User.Client.Prekey (ClientPrekey, PrekeyBundle)
import Wire.API.User.Search
import Wire.API.UserMap (UserMap)

-- | For conventions see /docs/developer/federation-api-conventions.md
--
-- Maybe this module should be called Brig
data BrigApi routes = BrigApi
  { getUserByHandle ::
      routes
        :- "get-user-by-handle"
        :> ReqBody '[JSON] Handle
        :> Post '[JSON] (Maybe UserProfile),
    getUsersByIds ::
      routes
        :- "get-users-by-ids"
        :> ReqBody '[JSON] [UserId]
        :> Post '[JSON] [UserProfile],
    claimPrekey ::
      routes
        :- "claim-prekey"
        :> ReqBody '[JSON] (UserId, ClientId)
        :> Post '[JSON] (Maybe ClientPrekey),
    claimPrekeyBundle ::
      routes
        :- "claim-prekey-bundle"
        :> ReqBody '[JSON] UserId
        :> Post '[JSON] PrekeyBundle,
    claimMultiPrekeyBundle ::
      routes
        :- "claim-multi-prekey-bundle"
        :> ReqBody '[JSON] UserClients
        :> Post '[JSON] UserClientPrekeyMap,
    searchUsers ::
      routes
        :- "search-users"
        -- FUTUREWORK(federation): do we want to perform some type-level validation like length checks?
        -- (handles can be up to 256 chars currently)
        :> ReqBody '[JSON] SearchRequest
        :> Post '[JSON] [Contact],
    getUserClients ::
      routes
        :- "get-user-clients"
        :> ReqBody '[JSON] GetUserClients
        :> Post '[JSON] (UserMap (Set PubClient)),
    sendConnectionAction ::
      routes
        :- "send-connection-action"
        :> OriginDomainHeader
        :> ReqBody '[JSON] NewConnectionRequest
        :> Post '[JSON] NewConnectionResponse,
    onUserDeleted ::
      routes
        :- "on-user-deleted-connections"
        :> OriginDomainHeader
        :> ReqBody '[JSON] UserDeletedConnectionsNotification
        :> Post '[JSON] EmptyResponse
  }
  deriving (Generic)

newtype SearchRequest = SearchRequest {term :: Text}
  deriving (Show, Eq, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform SearchRequest)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema SearchRequest)

instance ToSchema SearchRequest where
  schema =
    object "SearchRequest" $
      SearchRequest
        <$> term .= field "term" schema

newtype GetUserClients = GetUserClients
  { gucUsers :: [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema GetUserClients)

instance ToSchema GetUserClients where
  schema =
    object "GetUserClients" $
      GetUserClients
        <$> gucUsers .= field "users" (array schema)

-- NOTE: ConversationId for remote connections
--
-- The plan is to model the connect/one2one conversationId as deterministically derived from
-- the combination of both userIds and both domains. It may be in the domain
-- of the sending OR the receiving backend (with a 50/50 probability).
-- However at the level of the federation API, we are only concerned about
-- the question of which backend has the authority over the conversationId.
--
-- (Backend A should not prescribe backend B to use a certain UUID for its
-- conversation; as that could lead to a potential malicious override of an
-- existing conversation)
--
-- The deterministic conversation Id should be seen as a 'best effort'
-- attempt only. (we cannot guarantee a backend won't change the code in the
-- future)

data NewConnectionRequest = NewConnectionRequest
  { -- | The 'from' userId is understood to always have the domain of the backend making the connection request
    ncrFrom :: UserId,
    -- | The 'to' userId is understood to always have the domain of the receiving backend.
    ncrTo :: UserId,
    ncrAction :: RemoteConnectionAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConnectionRequest)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema NewConnectionRequest)

instance ToSchema NewConnectionRequest where
  schema =
    object "NewConnectionRequest" $
      NewConnectionRequest
        <$> ncrFrom .= field "from" schema
        <*> ncrTo .= field "to" schema
        <*> ncrAction .= field "action" schema

data RemoteConnectionAction
  = RemoteConnect
  | RemoteRescind
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConnectionAction)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema RemoteConnectionAction)

instance ToSchema RemoteConnectionAction where
  schema =
    enum @Text "RemoteConnectionAction" $
      mconcat
        [ element "connect" RemoteConnect,
          element "rescind" RemoteRescind
        ]

newtype NewConnectionRemoteAction = NewConnectionRemoteAction {unNCRA :: Maybe RemoteConnectionAction}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConnectionRemoteAction)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema NewConnectionRemoteAction)

instance ToSchema NewConnectionRemoteAction where
  schema =
    object "NewConnectionRemoteAction" $
      NewConnectionRemoteAction <$> unNCRA .= optField "remote_connection_action" (Just A.Null) schema

data NewConnectionResponse
  = NewConnectionResponseUserNotActivated
  | NewConnectionResponseOk NewConnectionRemoteAction
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConnectionResponse)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema NewConnectionResponse)

instance ToSchema NewConnectionResponse where
  schema =
    named "NewConnectionResponse" $
      tag _NewConnectionResponseUserNotActivated null_
        <> tag _NewConnectionResponseOk (unnamed schema)

type UserDeletedNotificationMaxConnections = 1000

data UserDeletedConnectionsNotification = UserDeletedConnectionsNotification
  { -- | This is qualified implicitly by the origin domain
    udcnUser :: UserId,
    -- | These are qualified implicitly by the target domain
    udcnConnections :: Range 1 UserDeletedNotificationMaxConnections [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConnectionsNotification)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema UserDeletedConnectionsNotification)

instance ToSchema UserDeletedConnectionsNotification where
  schema =
    object "UserDeletedConnectionsNotification" $
      UserDeletedConnectionsNotification
        <$> udcnUser .= field "user" schema
        <*> (fromRange . udcnConnections)
          .= field "connections" (rangedSchema sing sing (array schema))

type ServantAPI = ToServantApi BrigApi

swaggerDoc :: S.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)

-----------------------------------------------------------------------------
-- Prisms for the ToSchema instance for NewConnectionResponse

_NewConnectionResponseUserNotActivated ::
  Prism' NewConnectionResponse ()
_NewConnectionResponseUserNotActivated =
  prism
    (const NewConnectionResponseUserNotActivated)
    ( \action ->
        case action of
          NewConnectionResponseUserNotActivated -> Right ()
          _ -> Left action
    )
{-# INLINE _NewConnectionResponseUserNotActivated #-}

_NewConnectionResponseOk ::
  Prism' NewConnectionResponse NewConnectionRemoteAction
_NewConnectionResponseOk =
  prism
    NewConnectionResponseOk
    ( \action ->
        case action of
          NewConnectionResponseOk remoteAction -> Right remoteAction
          _ -> Left action
    )
{-# INLINE _NewConnectionResponseOk #-}
