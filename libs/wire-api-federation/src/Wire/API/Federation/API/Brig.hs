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

module Wire.API.Federation.API.Brig where

import Data.Aeson
import Data.Handle (Handle)
import Data.Id
import Data.Range
import Imports
import Servant.API
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (..))
import Wire.API.Federation.API.Common
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.Message (UserClients)
import Wire.API.User (UserProfile)
import Wire.API.User.Client (PubClient, UserClientPrekeyMap)
import Wire.API.User.Client.Prekey (ClientPrekey, PrekeyBundle)
import Wire.API.User.Search
import Wire.API.UserMap (UserMap)
import Wire.API.Util.Aeson (CustomEncoded (..))

newtype SearchRequest = SearchRequest {term :: Text}
  deriving (Show, Eq, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform SearchRequest)

instance ToJSON SearchRequest

instance FromJSON SearchRequest

data SearchResponse = SearchResponse
  { contacts :: [Contact],
    searchPolicy :: FederatedUserSearchPolicy
  }
  deriving (Show, Generic, Typeable)

instance ToJSON SearchResponse

instance FromJSON SearchResponse

-- | For conventions see /docs/developer/federation-api-conventions.md
type BrigApi =
  FedEndpoint "api-version" () VersionInfo
    :<|> FedEndpoint "get-user-by-handle" Handle (Maybe UserProfile)
    :<|> FedEndpoint "get-users-by-ids" [UserId] [UserProfile]
    :<|> FedEndpoint "claim-prekey" (UserId, ClientId) (Maybe ClientPrekey)
    :<|> FedEndpoint "claim-prekey-bundle" UserId PrekeyBundle
    :<|> FedEndpoint "claim-multi-prekey-bundle" UserClients UserClientPrekeyMap
    -- FUTUREWORK(federation): do we want to perform some type-level validation like length checks?
    -- (handles can be up to 256 chars currently)
    :<|> FedEndpoint "search-users" SearchRequest SearchResponse
    :<|> FedEndpoint "get-user-clients" GetUserClients (UserMap (Set PubClient))
    :<|> FedEndpoint "get-mls-clients" MLSClientsRequest (Set ClientId)
    :<|> FedEndpoint "send-connection-action" NewConnectionRequest NewConnectionResponse
    :<|> FedEndpoint "on-user-deleted-connections" UserDeletedConnectionsNotification EmptyResponse
    :<|> FedEndpoint "claim-key-packages" ClaimKeyPackageRequest (Maybe KeyPackageBundle)

newtype GetUserClients = GetUserClients
  { gucUsers :: [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetUserClients)

data MLSClientsRequest = MLSClientsRequest
  { mcrUserId :: UserId, -- implicitly qualified by the local domain
    mcrSignatureScheme :: SignatureSchemeTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSClientsRequest)

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
  deriving (FromJSON, ToJSON) via (CustomEncoded NewConnectionRequest)

data RemoteConnectionAction
  = RemoteConnect
  | RemoteRescind
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConnectionAction)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConnectionAction)

data NewConnectionResponse
  = NewConnectionResponseUserNotActivated
  | NewConnectionResponseOk (Maybe RemoteConnectionAction)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConnectionResponse)
  deriving (FromJSON, ToJSON) via (CustomEncoded NewConnectionResponse)

type UserDeletedNotificationMaxConnections = 1000

data UserDeletedConnectionsNotification = UserDeletedConnectionsNotification
  { -- | This is qualified implicitly by the origin domain
    udcnUser :: UserId,
    -- | These are qualified implicitly by the target domain
    udcnConnections :: Range 1 UserDeletedNotificationMaxConnections [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConnectionsNotification)
  deriving (FromJSON, ToJSON) via (CustomEncoded UserDeletedConnectionsNotification)

data ClaimKeyPackageRequest = ClaimKeyPackageRequest
  { -- | The user making the request, implictly qualified by the origin domain.
    ckprClaimant :: UserId,
    -- | The user whose key packages are being claimed, implictly qualified by
    -- the target domain.
    ckprTarget :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClaimKeyPackageRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClaimKeyPackageRequest)
