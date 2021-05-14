{-# LANGUAGE DerivingVia #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Public.Brig where

import Data.CommaSeparatedList (CommaSeparatedList)
import Data.Domain
import Data.Handle
import Data.Id as Id
import Data.Qualified (Qualified (..))
import Data.Range
import Data.Swagger hiding (Contact)
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.API.Generic
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Public
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Search (Contact, SearchResult)
import Wire.API.UserMap

type MaxUsersForListClientsBulk = 500

type CheckUserExistsResponse = [Empty200, Empty404]

data Api routes = Api
  { -- Note [document responses]
    --
    -- Ideally we want to document responses with UVerb and swagger, but this is
    -- currently not possible due to this issue:
    -- https://github.com/haskell-servant/servant/issues/1369

    -- See Note [ephemeral user sideeffect]
    --
    -- See Note [document responses]
    -- The responses looked like this:
    --   Doc.response 200 "User exists" Doc.end
    --   Doc.errorResponse userNotFound
    checkUserExistsUnqualified ::
      routes
        :- Summary "Check if a user ID exists (deprecated)"
        :> ZAuthServant
        :> "users"
        :> CaptureUserId "uid"
        :> UVerb 'HEAD '[JSON] CheckUserExistsResponse,
    -- See Note [ephemeral user sideeffect]
    --
    -- See Note [document responses]
    -- The responses looked like this:
    --   Doc.response 200 "User exists" Doc.end
    --   Doc.errorResponse userNotFound
    checkUserExistsQualified ::
      routes
        :- Summary "Check if a user ID exists"
        :> ZAuthServant
        :> "users"
        :> Capture "domain" Domain
        :> CaptureUserId "uid"
        :> UVerb 'HEAD '[JSON] CheckUserExistsResponse,
    -- See Note [ephemeral user sideeffect]
    --
    -- See Note [document responses]
    -- The responses looked like this:
    --   Doc.response 200 "User" Doc.end
    --   Doc.errorResponse userNotFound
    getUserUnqualified ::
      routes
        :- Summary "Get a user by UserId (deprecated)"
        :> ZAuthServant
        :> "users"
        :> CaptureUserId "uid"
        :> Get '[JSON] UserProfile,
    -- See Note [ephemeral user sideeffect]
    --
    -- See Note [document responses]
    -- The responses looked like this:
    --   Doc.response 200 "User" Doc.end
    --   Doc.errorResponse userNotFound
    getUserQualified ::
      routes
        :- Summary "Get a user by Domain and UserId"
        :> ZAuthServant
        :> "users"
        :> Capture "domain" Domain
        :> CaptureUserId "uid"
        :> Get '[JSON] UserProfile,
    getSelf ::
      routes
        :- Summary "Get your own profile"
        :> ZAuthServant
        :> "self"
        :> Get '[JSON] SelfProfile,
    -- See Note [document responses]
    -- The responses looked like this:
    --   Doc.returns (Doc.ref modelUserHandleInfo)
    --   Doc.response 200 "Handle info" Doc.end
    --   Doc.errorResponse handleNotFound
    getHandleInfoUnqualified ::
      routes
        :- Summary "(deprecated, use /search/contacts) Get information on a user handle"
        :> ZAuthServant
        :> "users"
        :> "handles"
        :> Capture' '[Description "The user handle"] "handle" Handle
        :> Get '[JSON] UserHandleInfo,
    -- See Note [document responses]
    -- The responses looked like this:
    --   Doc.returns (Doc.ref modelUserHandleInfo)
    --   Doc.response 200 "Handle info" Doc.end
    --   Doc.errorResponse handleNotFound
    getUserByHandleQualfied ::
      routes
        :- Summary "(deprecated, use /search/contacts) Get information on a user handle"
        :> ZAuthServant
        :> "users"
        :> "by-handle"
        :> Capture "domain" Domain
        :> Capture' '[Description "The user handle"] "handle" Handle
        :> Get '[JSON] UserProfile,
    -- See Note [ephemeral user sideeffect]
    listUsersByUnqualifiedIdsOrHandles ::
      routes
        :- Summary "List users (deprecated)"
        :> Description "The 'ids' and 'handles' parameters are mutually exclusive."
        :> ZAuthServant
        :> "users"
        :> QueryParam' [Optional, Strict, Description "User IDs of users to fetch"] "ids" (CommaSeparatedList UserId)
        :> QueryParam' [Optional, Strict, Description "Handles of users to fetch, min 1 and max 4 (the check for handles is rather expensive)"] "handles" (Range 1 4 (CommaSeparatedList Handle))
        :> Get '[JSON] [UserProfile],
    -- See Note [ephemeral user sideeffect]
    listUsersByIdsOrHandles ::
      routes
        :- Summary "List users"
        :> Description "The 'qualified_ids' and 'qualified_handles' parameters are mutually exclusive."
        :> ZAuthServant
        :> "list-users"
        :> ReqBody '[JSON] ListUsersQuery
        :> Post '[JSON] [UserProfile],
    getUserClientsUnqualified ::
      routes
        :- Summary "Get all of a user's clients (deprecated)."
        :> "users"
        :> CaptureUserId "uid"
        :> "clients"
        :> Get '[JSON] [PubClient],
    getUserClientsQualified ::
      routes
        :- Summary "Get all of a user's clients."
        :> "users"
        :> Capture "domain" Domain
        :> CaptureUserId "uid"
        :> "clients"
        :> Get '[JSON] [PubClient],
    getUserClientUnqualified ::
      routes
        :- Summary "Get a specific client of a user (deprecated)."
        :> "users"
        :> CaptureUserId "uid"
        :> "clients"
        :> CaptureClientId "client"
        :> Get '[JSON] PubClient,
    getUserClientQualified ::
      routes
        :- Summary "Get a specific client of a user."
        :> "users"
        :> Capture "domain" Domain
        :> CaptureUserId "uid"
        :> "clients"
        :> CaptureClientId "client"
        :> Get '[JSON] PubClient,
    listClientsBulk ::
      routes
        :- Summary "List all clients for a set of user ids (deprecated, use /users/list-clients/v2)"
        :> ZAuthServant
        :> "users"
        :> "list-clients"
        :> ReqBody '[JSON] (Range 1 MaxUsersForListClientsBulk [Qualified UserId])
        :> Post '[JSON] (QualifiedUserMap (Set PubClient)),
    listClientsBulkV2 ::
      routes
        :- Summary "List all clients for a set of user ids"
        :> ZAuthServant
        :> "users"
        :> "list-clients"
        :> "v2"
        :> ReqBody '[JSON] (LimitedQualifiedUserIdList MaxUsersForListClientsBulk)
        :> Post '[JSON] (WrappedQualifiedUserMap (Set PubClient)),
    getUsersPrekeysClientUnqualified ::
      routes
        :- Summary "(deprecated) Get a prekey for a specific client of a user."
        :> "users"
        :> CaptureUserId "uid"
        :> "prekeys"
        :> CaptureClientId "client"
        :> Get '[JSON] ClientPrekey,
    getUsersPrekeysClientQualified ::
      routes
        :- Summary "Get a prekey for a specific client of a user."
        :> "users"
        :> Capture "domain" Domain
        :> CaptureUserId "uid"
        :> "prekeys"
        :> CaptureClientId "client"
        :> Get '[JSON] ClientPrekey,
    getUsersPrekeyBundleUnqualified ::
      routes
        :- Summary "(deprecated) Get a prekey for each client of a user."
        :> "users"
        :> CaptureUserId "uid"
        :> "prekeys"
        :> Get '[JSON] PrekeyBundle,
    getUsersPrekeyBundleQualified ::
      routes
        :- Summary "Get a prekey for each client of a user."
        :> "users"
        :> Capture "domain" Domain
        :> CaptureUserId "uid"
        :> "prekeys"
        :> Get '[JSON] PrekeyBundle,
    getMultiUserPrekeyBundleUnqualified ::
      routes
        :- Summary
             "(deprecated)  Given a map of user IDs to client IDs return a \
             \prekey for each one. You can't request information for more users than \
             \maximum conversation size."
        :> "users"
        :> "prekeys"
        :> ReqBody '[JSON] UserClients
        :> Post '[JSON] (UserClientMap (Maybe Prekey)),
    getMultiUserPrekeyBundleQualified ::
      routes
        :- Summary
             "Given a map of domain to (map of user IDs to client IDs) return a \
             \prekey for each one. You can't request information for more users than \
             \maximum conversation size."
        :> "users"
        :> "list-prekeys"
        :> ReqBody '[JSON] QualifiedUserClients
        :> Post '[JSON] (QualifiedUserClientMap (Maybe Prekey)),
    searchContacts ::
      routes :- Summary "Search for users"
        :> ZAuthServant
        :> "search"
        :> "contacts"
        :> QueryParam' '[Required, Strict, Description "Search query"] "q" Text
        :> QueryParam' '[Optional, Strict, Description "Searched domain. Note: This is optional only for backwards compatibility, future versions will mandate this."] "domain" Domain
        :> QueryParam' '[Optional, Strict, Description "Number of results to return (min: 1, max: 500, default 15)"] "size" (Range 1 500 Int32)
        :> Get '[Servant.JSON] (SearchResult Contact)
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

swagger :: Swagger
swagger = toSwagger (Proxy @ServantAPI)
