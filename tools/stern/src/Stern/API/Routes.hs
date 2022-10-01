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

module Stern.API.Routes
  ( SternAPI,
    SternAPIInternal,
    SwaggerDocsAPI,
    swaggerDocsAPI,
    UserConnectionGroups (..),
  )
where

import Brig.Types.Intra (UserAccount)
import Control.Lens
import qualified Data.Aeson as A
import Data.Handle
import Data.Id
import qualified Data.Schema as Schema
import qualified Data.Swagger as S
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus)
import Wire.API.Routes.Named
import Wire.API.SwaggerHelper (cleanupSwagger)
import Wire.API.User
import Wire.API.User.Search

----------------------------------------------------------------------
-- routing tables

type SternAPIInternal =
  Named
    "status"
    ( "i"
        :> "status"
        :> Get '[JSON] NoContent
    )

type SternAPI =
  Named
    "suspend-user"
    ( Summary "Suspends user with this ID"
        :> "users"
        :> Capture "uid" UserId
        :> "suspend"
        :> Post '[JSON] NoContent
    )
    :<|> Named
           "unsuspend-user"
           ( Summary "Unsuspends user with this ID"
               :> "users"
               :> Capture "uid" UserId
               :> "unsuspend"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "get-users-by-email"
           ( Summary "Displays user's info given an email address"
               :> "users"
               :> QueryParam' [Required, Strict, Description "Email address"] "email" Email
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-phone"
           ( Summary "Displays user's info given a phone number"
               :> "users"
               :> QueryParam' [Required, Strict, Description "Phone number"] "phone" Phone
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-ids"
           ( Summary "Displays active users info given a list of ids"
               :> "users"
               :> QueryParam' [Required, Strict, Description "List of IDs of the users, separated by comma"] "ids" [UserId]
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-handles"
           ( Summary "Displays active users info given a list of handles"
               :> "users"
               :> QueryParam' [Required, Strict, Description "List of Handles of the users, without '@', separated by comma"] "ids" [Handle]
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-user-connections"
           ( Summary "Displays user's connections"
               :> "users"
               :> Capture "uid" UserId
               :> "connections"
               :> Get '[JSON] UserConnectionGroups
           )
    :<|> Named
           "get-users-connections"
           ( Summary "Displays connections of many users given a list of ids"
               :> "users"
               :> "connections"
               :> QueryParam' [Required, Strict, Description "List of IDs of the users, separated by comma"] "ids" [UserId]
               :> Get '[JSON] [ConnectionStatus]
           )
    :<|> Named
           "search-users"
           ( Summary "Search for users on behalf of"
               :> "users"
               :> Capture "uid" UserId
               :> "search"
               :> QueryParam' [Optional, Strict, Description "Search query"] "q (default \"\")" Text
               :> QueryParam' [Optional, Strict, Description "Number of results to return"] "size (min 1, max 100, default 10)" Int32
               :> Get '[JSON] (SearchResult Contact)
           )
    :<|> Named
           "revoke-identity"
           ( Summary "Revoke a verified user identity.  Specify exactly one of phone, email."
               :> Description
                    "Forcefully revokes a verified user identity. \
                    \WARNING: If the given identity is the only verified \
                    \user identity of an account, the account will be \
                    \deactivated (\"wireless\") and might thus become inaccessible. \
                    \If the given identity is not taken / verified, this is a no-op."
               :> "users"
               :> "revoke-identity"
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "put-email"
           ( Summary "Change a user's email address."
               :> Description
                    "The new e-mail address must be verified before the change takes effect."
               :> "users"
               :> Capture "uid" UserId
               :> "email"
               :> QueryParam' [Optional, Strict, Description "If set to true, a validation email will be sent to the new email address"] "validate" Bool
               :> Servant.ReqBody '[JSON] EmailUpdate
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "put-phone"
           ( Summary "Change a user's phone number."
               :> Description
                    "The new phone number must be verified before the change takes effect."
               :> "users"
               :> Capture "uid" UserId
               :> "phone"
               :> Servant.ReqBody '[JSON] PhoneUpdate
               :> Put '[JSON] NoContent
           )


-------------------------------------------------------------------------------
-- Swagger

type SwaggerDocsAPI = "backoffice" :> "api" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocsAPI :: Servant.Server SwaggerDocsAPI
swaggerDocsAPI =
  swaggerSchemaUIServer $
    toSwagger (Proxy @SternAPI)
      & S.info . S.title .~ "Stern API"
      & cleanupSwagger

----------------------------------------------------------------------
-- helpers

data UserConnectionGroups = UserConnectionGroups
  { ucgAccepted :: Int,
    ucgSent :: Int,
    ucgPending :: Int,
    ucgBlocked :: Int,
    ucgIgnored :: Int,
    ucgMissingLegalholdConsent :: Int,
    ucgTotal :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema.Schema UserConnectionGroups

instance Schema.ToSchema UserConnectionGroups where
  schema =
    Schema.object "UserConnectionGroups" $
      UserConnectionGroups
        <$> ucgAccepted Schema..= Schema.field "ucgAccepted" Schema.schema
        <*> ucgSent Schema..= Schema.field "ucgSent" Schema.schema
        <*> ucgPending Schema..= Schema.field "ucgPending" Schema.schema
        <*> ucgBlocked Schema..= Schema.field "ucgBlocked" Schema.schema
        <*> ucgIgnored Schema..= Schema.field "ucgIgnored" Schema.schema
        <*> ucgMissingLegalholdConsent Schema..= Schema.field "ucgMissingLegalholdConsent" Schema.schema
        <*> ucgTotal Schema..= Schema.field "ucgTotal" Schema.schema
