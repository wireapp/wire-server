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

import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Handle (Handle)
import Data.Id (ClientId, UserId)
import Imports
import qualified Network.HTTP.Types as HTTP
import Servant.API
import Servant.API.Generic
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.Message (UserClientMap, UserClients)
import Wire.API.User (UserProfile)
import Wire.API.User.Client.Prekey (ClientPrekey, Prekey, PrekeyBundle)
import Wire.API.User.Search

-- Maybe this module should be called Brig
data Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> QueryParam' '[Required, Strict] "handle" Handle
        :> Get '[JSON] UserProfile,
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
        :> QueryParam' '[Required, Strict] "uid" UserId
        :> QueryParam' '[Required, Strict] "client" ClientId
        :> Get '[JSON] (Maybe ClientPrekey),
    getPrekeyBundle ::
      routes
        :- "federation"
        :> "users"
        :> "prekey-bundle"
        :> QueryParam' '[Required, Strict] "uid" UserId
        :> Get '[JSON] PrekeyBundle,
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
        -- FUTUREWORK(federation): change this to a POST with a body,
        -- rather than a query parameter, after deciding on a general pattern here
        :> QueryParam' '[Required, Strict] "q" Text
        :> Get '[JSON] (SearchResult Contact)
  }
  deriving (Generic)

-- FUTUREWORK(federation): Idea: by keeping the functions to construct a Request and the API definitions in the same place,
-- we can:
-- - more easily make sure their definitions match
-- - probably add their path segments to a list for validation purposes to guard against path traversals.

-- FUTUREWORK(federation): I think we should make the federation/ prefix explicit here and not add it in services/federator/src/Federator/Federate.hs
mkGetUserInfoByHandle :: Handle -> Proto.Request
mkGetUserInfoByHandle handle =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.GET)
    "users/by-handle"
    [Proto.QueryParam "handle" (toByteString' handle)]
    mempty

mkClaimPrekey :: UserId -> ClientId -> Proto.Request
mkClaimPrekey user client =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.GET)
    "users/prekey"
    [ Proto.QueryParam "uid" (toByteString' user),
      Proto.QueryParam "client" (toByteString' client)
    ]
    mempty

mkClaimPrekeyBundle :: UserId -> Proto.Request
mkClaimPrekeyBundle user =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.GET)
    "users/prekey-bundle"
    [Proto.QueryParam "uid" (toByteString' user)]
    mempty

mkClaimMultiPrekeyBundle :: UserClients -> Proto.Request
mkClaimMultiPrekeyBundle uc =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.POST)
    "users/multi-prekey-bundle"
    []
    (LBS.toStrict (Aeson.encode uc))

mkGetUsersByIds :: [UserId] -> Proto.Request
mkGetUsersByIds uids =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.POST)
    "users/get-by-ids"
    []
    (LBS.toStrict $ Aeson.encode uids)

-- FUTUREWORK: Can we write a test which makes use of mkSearchUsers against the Api in this file?
mkSearchUsers :: Text -> Proto.Request
mkSearchUsers searchTerm =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.GET)
    "search/users"
    [Proto.QueryParam "q" (toByteString' searchTerm)]
    -- FUTUREWORK(federation): do we want to pass other parameters like the number of results?
    mempty
