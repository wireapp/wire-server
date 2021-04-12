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
import Wire.API.User (UserProfile)
import Wire.API.User.Client.Prekey (ClientPrekey)

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
        :> Get '[JSON] (Maybe ClientPrekey)
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

mkGetUsersByIds :: [UserId] -> Proto.Request
mkGetUsersByIds uids =
  Proto.Request
    Proto.Brig
    (Proto.HTTPMethod HTTP.POST)
    "users/by-id"
    []
    (LBS.toStrict $ Aeson.encode uids)
