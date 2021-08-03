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

module Wire.API.Routes.Public.Brig where

import Data.CommaSeparatedList (CommaSeparatedList)
import Data.Domain
import Data.Handle
import Data.Id as Id
import Data.Misc (IpAddr)
import Data.Qualified (Qualified (..))
import Data.Range
import Data.Swagger hiding (Contact, Header)
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.API.Generic
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Wire.API.ErrorDescription
  ( CanThrow,
    EmptyErrorForLegacyReasons,
    HandleNotFound,
    MalformedPrekeys,
    MissingAuth,
    TooManyClients,
    UserNotFound,
  )
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Public (ZConn, ZUser)
import Wire.API.Routes.QualifiedCapture
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Search (Contact, SearchResult)
import Wire.API.UserMap

type MaxUsersForListClientsBulk = 500

type GetUserVerb =
  MultiVerb
    'GET
    '[JSON]
    '[ UserNotFound,
       Respond 200 "User found" UserProfile
     ]
    (Maybe UserProfile)

type CaptureUserId name = Capture' '[Description "User Id"] name UserId

type QualifiedCaptureUserId name = QualifiedCapture' '[Description "User Id"] name UserId

type CaptureClientId name = Capture' '[Description "ClientId"] name ClientId

type NewClientResponse = Headers '[Header "Location" ClientId] Client

data Api routes = Api
  { -- See Note [ephemeral user sideeffect]
    getUserUnqualified ::
      routes
        :- Summary "Get a user by UserId (deprecated)"
        :> ZUser
        :> "users"
        :> CaptureUserId "uid"
        :> GetUserVerb,
    -- See Note [ephemeral user sideeffect]
    getUserQualified ::
      routes
        :- Summary "Get a user by Domain and UserId"
        :> ZUser
        :> "users"
        :> QualifiedCaptureUserId "uid"
        :> GetUserVerb,
    getSelf ::
      routes
        :- Summary "Get your own profile"
        :> ZUser
        :> "self"
        :> Get '[JSON] SelfProfile,
    getHandleInfoUnqualified ::
      routes
        :- Summary "(deprecated, use /search/contacts) Get information on a user handle"
        :> ZUser
        :> "users"
        :> "handles"
        :> Capture' '[Description "The user handle"] "handle" Handle
        :> MultiVerb
             'GET
             '[JSON]
             '[ HandleNotFound,
                Respond 200 "User found" UserHandleInfo
              ]
             (Maybe UserHandleInfo),
    getUserByHandleQualified ::
      routes
        :- Summary "(deprecated, use /search/contacts) Get information on a user handle"
        :> ZUser
        :> "users"
        :> "by-handle"
        :> QualifiedCapture' '[Description "The user handle"] "handle" Handle
        :> MultiVerb
             'GET
             '[JSON]
             '[ HandleNotFound,
                Respond 200 "User found" UserProfile
              ]
             (Maybe UserProfile),
    -- See Note [ephemeral user sideeffect]
    listUsersByUnqualifiedIdsOrHandles ::
      routes
        :- Summary "List users (deprecated)"
        :> Description "The 'ids' and 'handles' parameters are mutually exclusive."
        :> ZUser
        :> "users"
        :> QueryParam' [Optional, Strict, Description "User IDs of users to fetch"] "ids" (CommaSeparatedList UserId)
        :> QueryParam' [Optional, Strict, Description "Handles of users to fetch, min 1 and max 4 (the check for handles is rather expensive)"] "handles" (Range 1 4 (CommaSeparatedList Handle))
        :> Get '[JSON] [UserProfile],
    -- See Note [ephemeral user sideeffect]
    listUsersByIdsOrHandles ::
      routes
        :- Summary "List users"
        :> Description "The 'qualified_ids' and 'qualified_handles' parameters are mutually exclusive."
        :> ZUser
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
        :> QualifiedCaptureUserId "uid"
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
        :> QualifiedCaptureUserId "uid"
        :> "clients"
        :> CaptureClientId "client"
        :> Get '[JSON] PubClient,
    listClientsBulk ::
      routes
        :- Summary "List all clients for a set of user ids (deprecated, use /users/list-clients/v2)"
        :> ZUser
        :> "users"
        :> "list-clients"
        :> ReqBody '[JSON] (Range 1 MaxUsersForListClientsBulk [Qualified UserId])
        :> Post '[JSON] (QualifiedUserMap (Set PubClient)),
    listClientsBulkV2 ::
      routes
        :- Summary "List all clients for a set of user ids"
        :> ZUser
        :> "users"
        :> "list-clients"
        :> "v2"
        :> ReqBody '[JSON] (LimitedQualifiedUserIdList MaxUsersForListClientsBulk)
        :> Post '[JSON] (WrappedQualifiedUserMap (Set PubClient)),
    getUsersPrekeysClientUnqualified ::
      routes
        :- Summary "(deprecated) Get a prekey for a specific client of a user."
        :> ZUser
        :> "users"
        :> CaptureUserId "uid"
        :> "prekeys"
        :> CaptureClientId "client"
        :> Get '[JSON] ClientPrekey,
    getUsersPrekeysClientQualified ::
      routes
        :- Summary "Get a prekey for a specific client of a user."
        :> ZUser
        :> "users"
        :> QualifiedCaptureUserId "uid"
        :> "prekeys"
        :> CaptureClientId "client"
        :> Get '[JSON] ClientPrekey,
    getUsersPrekeyBundleUnqualified ::
      routes
        :- Summary "(deprecated) Get a prekey for each client of a user."
        :> ZUser
        :> "users"
        :> CaptureUserId "uid"
        :> "prekeys"
        :> Get '[JSON] PrekeyBundle,
    getUsersPrekeyBundleQualified ::
      routes
        :- Summary "Get a prekey for each client of a user."
        :> ZUser
        :> "users"
        :> QualifiedCaptureUserId "uid"
        :> "prekeys"
        :> Get '[JSON] PrekeyBundle,
    getMultiUserPrekeyBundleUnqualified ::
      routes
        :- Summary
             "(deprecated)  Given a map of user IDs to client IDs return a \
             \prekey for each one. You can't request information for more users than \
             \maximum conversation size."
        :> ZUser
        :> "users"
        :> "prekeys"
        :> ReqBody '[JSON] UserClients
        :> Post '[JSON] UserClientPrekeyMap,
    getMultiUserPrekeyBundleQualified ::
      routes
        :- Summary
             "Given a map of domain to (map of user IDs to client IDs) return a \
             \prekey for each one. You can't request information for more users than \
             \maximum conversation size."
        :> ZUser
        :> "users"
        :> "list-prekeys"
        :> ReqBody '[JSON] QualifiedUserClients
        :> Post '[JSON] QualifiedUserClientPrekeyMap,
    -- User Client API ----------------------------------------------------

    -- This endpoint can lead to the following events being sent:
    -- - ClientAdded event to self
    -- - ClientRemoved event to self, if removing old clients due to max number
    addClient ::
      routes :- Summary "Register a new client"
        :> CanThrow TooManyClients
        :> CanThrow MissingAuth
        :> CanThrow MalformedPrekeys
        :> ZUser
        :> ZConn
        :> "clients"
        :> Header "X-Forwarded-For" IpAddr
        :> ReqBody '[JSON] NewClient
        :> Verb 'POST 201 '[JSON] NewClientResponse,
    updateClient ::
      routes :- Summary "Update a registered client"
        :> CanThrow MalformedPrekeys
        :> ZUser
        :> "clients"
        :> CaptureClientId "client"
        :> ReqBody '[JSON] UpdateClient
        :> MultiVerb 'PUT '[] '[RespondEmpty 200 "Client updated"] (),
    -- This endpoint can lead to the following events being sent:
    -- - ClientRemoved event to self
    deleteClient ::
      routes :- Summary "Delete an existing client"
        :> ZUser
        :> ZConn
        :> "clients"
        :> CaptureClientId "client"
        :> ReqBody '[JSON] RmClient
        :> MultiVerb 'DELETE '[] '[RespondEmpty 200 "Client deleted"] (),
    listClients ::
      routes :- Summary "List the registered clients"
        :> ZUser
        :> "clients"
        :> Get '[JSON] [Client],
    getClient ::
      routes :- Summary "Get a registered client by ID"
        :> ZUser
        :> "clients"
        :> CaptureClientId "client"
        :> MultiVerb
             'GET
             '[JSON]
             '[ EmptyErrorForLegacyReasons 404 "Client not found",
                Respond 200 "Client found" Client
              ]
             (Maybe Client),
    getClientCapabilities ::
      routes :- Summary "Read back what the client has been posting about itself"
        :> ZUser
        :> "clients"
        :> CaptureClientId "client"
        :> "capabilities"
        :> Get '[JSON] ClientCapabilityList,
    getClientPrekeys ::
      routes :- Summary "List the remaining prekey IDs of a client"
        :> ZUser
        :> "clients"
        :> CaptureClientId "client"
        :> "prekeys"
        :> Get '[JSON] [PrekeyId],
    searchContacts ::
      routes :- Summary "Search for users"
        :> ZUser
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
