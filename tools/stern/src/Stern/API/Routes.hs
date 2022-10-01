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
    doubleMaybeToEither,
  )
where

import Brig.Types.Intra (UserAccount)
import Control.Lens
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import Data.Handle
import Data.Id
import qualified Data.Schema as Schema
import qualified Data.Swagger as S
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai.Utilities
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus)
import qualified Wire.API.Routes.Internal.Brig.EJPD as EJPD
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
    :<|> Named
           "legacy-api-docs"
           ( "stern"
               :> "api-docs"
               :> QueryParam' [Required, Strict, Description "Base URL"] "base_url" Text
               -- we throw the old swagger docs as a exception with status 200 so we don't
               -- have to implement its type and can give 'NoContent' as the response body
               -- type here.
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
               :> QueryParam' [Required, Strict, Description "List of Handles of the users, without '@', separated by comma"] "handles" [Handle]
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
    :<|> Named
           "delete-user"
           ( Summary "Delete a user (irrevocable!)"
               :> Description
                    "Email or Phone must match UserId's (to prevent copy/paste mistakes).  Use exactly one of the two query params."
               :> "users"
               :> Capture "uid" UserId
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Delete '[JSON] NoContent
           )
    :<|> Named
           "suspend-team"
           ( Summary "Suspend a team."
               :> "teams"
               :> Capture "tid" TeamId
               :> "suspend"
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "unsuspend-team"
           ( Summary "Set a team status to 'Active', independently on previous status.  (Cannot be used to un-delete teams, though.)"
               :> "teams"
               :> Capture "tid" TeamId
               :> "unsuspend"
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "delete-team"
           ( Summary "Delete a team (irrevocable!). You can only delete teams with 1 user unless you use the 'force' query flag"
               :> Description
                    "The email address of the user must be provided to prevent copy/paste mistakes.\n\
                    \The force query flag can be used to delete teams with more than one user. \
                    \CAUTION: FORCE DELETE WILL PERMANENTLY DELETE ALL TEAM MEMBERS! \
                    \CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."
               :> "teams"
               :> Capture "tid" TeamId
               :> QueryParam' [Optional, Strict, Description "THIS WILL PERMANENTLY DELETE ALL TEAM MEMBERS! CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."] "force" Bool
               :> QueryParam' [Optional, Strict, Description "Matching verified remaining user address"] "email" Email
               :> Delete '[JSON] NoContent
           )
    :<|> Named
           "ejpd-info"
           ( Summary "internal wire.com process: https://wearezeta.atlassian.net/wiki/spaces/~463749889/pages/256738296/EJPD+official+requests+process"
               :> "ejpd-info"
               :> QueryParam' [Optional, Strict, Description "If 'true', this gives you more more exhaustive information about this user (including social network)"] "include_contacts" Bool
               :> QueryParam' [Required, Strict, Description "Handles of the users, separated by commas (NB: all chars need to be lower case!)"] "handles" [Handle]
               :> Delete '[JSON] EJPD.EJPDResponseBody
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

doubleMaybeToEither :: Monad m => LText -> Maybe a -> Maybe b -> ExceptT Error m (Either a b)
doubleMaybeToEither _ (Just a) Nothing = pure $ Left a
doubleMaybeToEither _ Nothing (Just b) = pure $ Right b
doubleMaybeToEither msg _ _ = throwE $ mkError status400 "either-params" ("Must use exactly one of two query params: " <> msg)
