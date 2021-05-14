
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
import Wire.API.Federation.Client (FederationClientError, FederatorClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.Message (UserClientMap, UserClients)
import Wire.API.User (UserProfile)
import Wire.API.User.Client.Prekey (ClientPrekey, Prekey, PrekeyBundle)
import Wire.API.User.Search

newtype SearchRequest = SearchRequest {term :: Text}
  deriving (Show, Eq, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform SearchRequest)

instance ToJSON SearchRequest

instance FromJSON SearchRequest

-- Maybe this module should be called Brig
data Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> ReqBody '[JSON] Handle
        :> Post '[JSON] (Maybe UserProfile),
    getUsersByIds ::
      routes
        :- "federation"
        :> "users"
        :> "get-by-ids"
        :> ReqBody '[JSON] [UserId]
        :> Post '[JSON] [UserProfile],
    claimPrekey ::
      routes
        :- "federation"
        :> "users"
        :> "prekey"
        :> ReqBody '[JSON] (UserId, ClientId)
        :> Post '[JSON] (Maybe ClientPrekey),
    getPrekeyBundle ::
      routes
        :- "federation"
        :> "users"
        :> "prekey-bundle"
        :> ReqBody '[JSON] UserId
        :> Post '[JSON] PrekeyBundle,
    getMultiPrekeyBundle ::
      routes
        :- "federation"
        :> "users"
        :> "multi-prekey-bundle"
        :> ReqBody '[JSON] UserClients
        :> Post '[JSON] (UserClientMap (Maybe Prekey)),
    searchUsers ::
      routes
        :- "federation"
        :> "search"
        :> "users"
        -- FUTUREWORK(federation): do we want to perform some type-level validation like length checks?
        -- (handles can be up to 256 chars currently)
        :> ReqBody '[JSON] SearchRequest
        :> Post '[JSON] (SearchResult Contact)
  }
  deriving (Generic)

clientRoutes :: (MonadError FederationClientError m, MonadIO m) => Api (AsClientT (FederatorClient 'Proto.Brig m))
clientRoutes = genericClient
