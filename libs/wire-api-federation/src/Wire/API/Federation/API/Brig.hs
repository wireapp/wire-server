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
import Data.Handle (Handle)
import Imports
import Servant.API
import Servant.API.Generic
import Servant.Client.Generic (AsClientT, genericClient)
import Wire.API.Federation.Client (FederationClientError, FederatorClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.User (UserProfile)
import Wire.API.User.Search

-- Maybe this module should be called Brig
data Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> QueryParam' '[Required, Strict] "handle" Handle
        :> Get '[JSON] (Maybe UserProfile),
    searchUsers ::
      routes
        :- "federation"
        :> "search"
        :> "users"
        -- FUTUREWORK(federation): do we want to perform some type-level validation like length checks?
        -- (handles can be up to 256 chars currently)
        -- FUTUREWORK(federation): change this to a POST with a body,
        -- rather than a query parameter, after deciding on a general pattern here
        :> QueryParam' '[Required, Strict] "q" Text
        :> Get '[JSON] (SearchResult Contact)
  }
  deriving (Generic)

clientRoutes :: (MonadError FederationClientError m, MonadIO m) => Api (AsClientT (FederatorClient 'Proto.Brig m))
clientRoutes = genericClient
