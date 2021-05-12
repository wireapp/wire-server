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

import Control.Lens ( (<>~), (?~) )
import Data.Aeson ( ToJSON(toJSON) )
import Data.CommaSeparatedList ( CommaSeparatedList )
import Data.Domain ( Domain )
import Data.Handle ( Handle )
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
    ( singleton )
import Data.Id as Id ( ClientId, UserId )
import Data.Proxy ( Proxy(Proxy) )
import Data.Qualified ( Qualified )
import Data.Range ( Range )
import Data.Swagger
import Imports
    ( ($), Generic, Maybe(Just), (.), Set, Text, (<&>), (&) )
import Servant.API
    ( type (:>),
      HasStatus,
      Summary,
      UVerb,
      JSON,
      Capture,
      Get,
      Capture',
      Description,
      QueryParam',
      Optional,
      Strict,
      ReqBody,
      Post,
      Header',
      Required,
      WithStatus,
      StdMethod(HEAD) )
import Servant.API.Generic ((:-), ToServantApi)
import Servant.Swagger ( HasSwagger(..) )
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Servant.Server
    ( HasContextEntry,
      type (.++),
      DefaultErrorFormatters,
      ErrorFormatters,
      HasServer(..) )
import Wire.API.User
    ( LimitedQualifiedUserIdList,
      ListUsersQuery,
      SelfProfile,
      UserProfile )
import Wire.API.User.Activation ()
import Wire.API.User.Auth ()
import Wire.API.User.Client
    ( PubClient,
      QualifiedUserClientMap,
      QualifiedUserClients,
      UserClientMap,
      UserClients )
import Wire.API.User.Client.Prekey
    ( ClientPrekey, Prekey, PrekeyBundle )
import Wire.API.User.Handle ( UserHandleInfo )
import Wire.API.UserMap
    ( QualifiedUserMap, WrappedQualifiedUserMap )

-- | This type exists for the special 'HasSwagger' and 'HasServer' instances. It
-- shows the "Authorization" header in the swagger docs, but expects the
-- "Z-Auth" header in the server. This helps keep the swagger docs usable
-- through nginz.
data ZAuthServant

type InternalAuth = Header' '[Required, Strict] "Z-User" UserId

instance HasSwagger api => HasSwagger (ZAuthServant :> api) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & securityDefinitions <>~ InsOrdHashMap.singleton "ZAuth" secScheme
      & security <>~ [SecurityRequirement $ InsOrdHashMap.singleton "ZAuth" []]
    where
      secScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader),
            _securitySchemeDescription = Just "Must be a token retrieved by calling 'POST /login' or 'POST /access'. It must be presented in this format: 'Bearer \\<token\\>'."
          }

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    HasServer api ctx
  ) =>
  HasServer (ZAuthServant :> api) ctx
  where
  type ServerT (ZAuthServant :> api) m = ServerT (InternalAuth :> api) m

  route _ = route (Proxy @(InternalAuth :> api))
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @(InternalAuth :> api)) pc nt s


type CaptureUserId name = Capture' '[Description "User Id"] name UserId

type CaptureClientId name = Capture' '[Description "ClientId"] name ClientId

type MaxUsersForListClientsBulk = 500

type CheckUserExistsResponse = [Empty200, Empty404]

data Empty200 = Empty200
  deriving (Generic)
  deriving (HasStatus) via (WithStatus 200 Empty200)

instance ToSchema Empty200 where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToJSON Empty200 where
  toJSON _ = toJSON ("" :: Text)

data Empty404 = Empty404
  deriving (Generic)
  deriving (HasStatus) via (WithStatus 404 Empty404)

instance ToJSON Empty404 where
  toJSON _ = toJSON ("" :: Text)

instance ToSchema Empty404 where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text) <&> (schema . description ?~ "user not found")

data Api routes = Api
  { 
    -- Note [document responses]
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
        :> Post '[JSON] (QualifiedUserClientMap (Maybe Prekey))
  }

type OutsideWorldAPI = ToServantApi Api

swagger :: Swagger
swagger = toSwagger (Proxy @OutsideWorldAPI)
