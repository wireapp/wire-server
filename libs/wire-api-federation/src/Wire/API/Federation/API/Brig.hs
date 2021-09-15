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

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Handle (Handle)
import Data.Id (ClientId, UserId)
import Imports
import Servant.API
import Servant.API.Generic
import Servant.Client.Generic (AsClientT, genericClient)
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (..))
import Wire.API.Federation.Client (FederationClientFailure, FederatorClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
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

-- | For conventions see /docs/developer/federation-api-conventions.md
--
-- Maybe this module should be called Brig
data Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "get-user-by-handle"
        :> ReqBody '[JSON] Handle
        :> Post '[JSON] (Maybe UserProfile),
    getUsersByIds ::
      routes
        :- "federation"
        :> "get-users-by-ids"
        :> ReqBody '[JSON] [UserId]
        :> Post '[JSON] [UserProfile],
    claimPrekey ::
      routes
        :- "federation"
        :> "claim-prekey"
        :> ReqBody '[JSON] (UserId, ClientId)
        :> Post '[JSON] (Maybe ClientPrekey),
    claimPrekeyBundle ::
      routes
        :- "federation"
        :> "claim-prekey-bundle"
        :> ReqBody '[JSON] UserId
        :> Post '[JSON] PrekeyBundle,
    claimMultiPrekeyBundle ::
      routes
        :- "federation"
        :> "claim-multi-prekey-bundle"
        :> ReqBody '[JSON] UserClients
        :> Post '[JSON] UserClientPrekeyMap,
    searchUsers ::
      routes
        :- "federation"
        :> "search-users"
        -- FUTUREWORK(federation): do we want to perform some type-level validation like length checks?
        -- (handles can be up to 256 chars currently)
        :> ReqBody '[JSON] SearchRequest
        :> Post '[JSON] [Contact],
    getUserClients ::
      routes
        :- "federation"
        :> "get-user-clients"
        :> ReqBody '[JSON] GetUserClients
        :> Post '[JSON] (UserMap (Set PubClient))
  }
  deriving (Generic)

newtype GetUserClients = GetUserClients
  { gucUsers :: [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetUserClients)

clientRoutes :: (MonadError FederationClientFailure m, MonadIO m) => Api (AsClientT (FederatorClient 'Proto.Brig m))
clientRoutes = genericClient
