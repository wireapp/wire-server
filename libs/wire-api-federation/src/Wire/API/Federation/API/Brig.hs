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

module Wire.API.Federation.API.Brig
  ( module Notifications,
    module Wire.API.Federation.API.Brig,
  )
where

import Data.Aeson
import Data.Domain (Domain)
import Data.Handle (Handle)
import Data.Id
import Data.OpenApi (OpenApi, ToSchema)
import Data.Proxy (Proxy (Proxy))
import Imports
import Servant.API
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Test.QuickCheck (Arbitrary)
import Wire.API.Federation.API.Brig.Notifications as Notifications
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.SpecialiseToVersion
import Wire.API.User (UserProfile)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey (ClientPrekey, PrekeyBundle)
import Wire.API.User.Search
import Wire.API.UserMap (UserMap)
import Wire.API.Util.Aeson (CustomEncoded (..))
import Wire.API.VersionInfo
import Wire.Arbitrary (GenericUniform (..))

data SearchRequest = SearchRequest
  { term :: Text,
    -- | The searcher's team ID, used to matched against the remote backend's team federation policy.
    from :: Maybe TeamId,
    -- | The remote teams that the calling backend is allowed to federate with.
    onlyInTeams :: Maybe [TeamId]
  }
  deriving (Show, Eq, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform SearchRequest)

instance ToJSON SearchRequest

instance FromJSON SearchRequest

instance ToSchema SearchRequest

data SearchResponse = SearchResponse
  { contacts :: [Contact],
    searchPolicy :: FederatedUserSearchPolicy
  }
  deriving (Show, Generic, Typeable)

instance ToJSON SearchResponse

instance FromJSON SearchResponse

instance ToSchema SearchResponse

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
    :<|> FedEndpointWithMods '[Until V1] (Versioned 'V0 "get-mls-clients") MLSClientsRequestV0 (Set ClientInfo)
    :<|> FedEndpointWithMods '[From V1] "get-mls-clients" MLSClientsRequest (Set ClientInfo)
    :<|> FedEndpointWithMods '[From V2] "get-mls-client" MLSClientRequest ClientInfo
    :<|> FedEndpoint "send-connection-action" NewConnectionRequest NewConnectionResponse
    :<|> FedEndpoint "claim-key-packages" ClaimKeyPackageRequest (Maybe KeyPackageBundle)
    :<|> FedEndpoint "get-not-fully-connected-backends" DomainSet NonConnectedBackends
    -- All the notification endpoints that go through the queue-based
    -- federation client ('fedQueueClient').
    :<|> BrigNotificationAPI

newtype DomainSet = DomainSet
  { domains :: Set Domain
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded DomainSet)

instance ToSchema DomainSet

newtype NonConnectedBackends = NonConnectedBackends
  -- TODO:
  -- The encoding rules that were in place would make this "connectedBackends" over the wire.
  -- I do not think that this was intended, so I'm leaving this note as it will be an API break.
  { nonConnectedBackends :: Set Domain
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded NonConnectedBackends)

instance ToSchema NonConnectedBackends

newtype GetUserClients = GetUserClients
  { users :: [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetUserClients)

instance ToSchema GetUserClients

data MLSClientsRequestV0 = MLSClientsRequestV0
  { userId :: UserId, -- implicitly qualified by the local domain
    signatureScheme :: SignatureSchemeTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSClientsRequestV0)

instance ToSchema MLSClientsRequestV0

data MLSClientsRequest = MLSClientsRequest
  { userId :: UserId, -- implicitly qualified by the local domain
    cipherSuite :: CipherSuite
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSClientsRequest)

instance ToSchema MLSClientsRequest

data MLSClientRequest = MLSClientRequest
  { userId :: UserId, -- implicitly qualified by the local domain
    clientId :: ClientId,
    cipherSuite :: CipherSuite
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSClientRequest)

instance ToSchema MLSClientRequest

mlsClientsRequestToV0 :: MLSClientsRequest -> MLSClientsRequestV0
mlsClientsRequestToV0 mcr =
  MLSClientsRequestV0
    { userId = mcr.userId,
      signatureScheme = Ed25519
    }

mlsClientsRequestFromV0 :: MLSClientsRequestV0 -> MLSClientsRequest
mlsClientsRequestFromV0 mcr =
  MLSClientsRequest
    { userId = mcr.userId,
      cipherSuite = tagCipherSuite MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
    }

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
    from :: UserId,
    -- | The team ID of the 'from' user. If the user is not in a team, it is set
    -- to 'Nothing'. It is implicitly qualified the same as the 'from' user.
    fromTeam :: Maybe TeamId,
    -- | The 'to' userId is understood to always have the domain of the receiving backend.
    to :: UserId,
    action :: RemoteConnectionAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConnectionRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded NewConnectionRequest)

instance ToSchema NewConnectionRequest

data RemoteConnectionAction
  = RemoteConnect
  | RemoteRescind
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConnectionAction)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConnectionAction)

instance ToSchema RemoteConnectionAction

data NewConnectionResponse
  = NewConnectionResponseUserNotActivated
  | NewConnectionResponseNotFederating
  | NewConnectionResponseOk (Maybe RemoteConnectionAction)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConnectionResponse)
  deriving (FromJSON, ToJSON) via (CustomEncoded NewConnectionResponse)

instance ToSchema NewConnectionResponse

data ClaimKeyPackageRequest = ClaimKeyPackageRequest
  { -- | The user making the request, implictly qualified by the origin domain.
    claimant :: UserId,
    -- | The user whose key packages are being claimed, implictly qualified by
    -- the target domain.
    target :: UserId,
    -- | The ciphersuite of the key packages being claimed.
    cipherSuite :: CipherSuite
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClaimKeyPackageRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClaimKeyPackageRequest)

instance ToSchema ClaimKeyPackageRequest

swaggerDoc :: OpenApi
swaggerDoc = toOpenApi (Proxy @(SpecialiseToVersion 'V1 BrigApi))
