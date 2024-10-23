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

module Wire.API.Routes.Public.Brig.Services where

import Data.Id as Id
import Data.Range
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Wire.API.Error (CanThrow)
import Wire.API.Error.Brig
import Wire.API.Provider.Service qualified as Public
import Wire.API.Provider.Service.Tag
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public

type ServicesAPI =
  Named
    "post-provider-services"
    ( Summary "Create a new service"
        :> CanThrow 'AccessDenied
        :> CanThrow 'InvalidServiceKey
        :> ZProvider
        :> "provider"
        :> "services"
        :> ReqBody '[JSON] Public.NewService
        :> MultiVerb1 'POST '[JSON] (Respond 201 "" Public.NewServiceResponse)
    )
    :<|> Named
           "get-provider-services"
           ( Summary "List provider services"
               :> CanThrow 'AccessDenied
               :> ZProvider
               :> "provider"
               :> "services"
               :> Get '[JSON] [Public.Service]
           )
    :<|> Named
           "get-provider-services-by-service-id"
           ( Summary "Get provider service by service id"
               :> CanThrow 'AccessDenied
               :> CanThrow 'ServiceNotFound
               :> ZProvider
               :> "provider"
               :> "services"
               :> Capture "service-id" ServiceId
               :> Get '[JSON] Public.Service
           )
    :<|> Named
           "put-provider-services-by-service-id"
           ( Summary "Update provider service"
               :> CanThrow 'AccessDenied
               :> CanThrow 'ServiceNotFound
               :> CanThrow 'ProviderNotFound
               :> ZProvider
               :> "provider"
               :> "services"
               :> Capture "service-id" ServiceId
               :> ReqBody '[JSON] Public.UpdateService
               :> MultiVerb1 'PUT '[PlainText, JSON] (RespondEmpty 200 "Provider service updated")
           )
    :<|> Named
           "put-provider-services-connection-by-service-id"
           ( Summary "Update provider service connection"
               :> CanThrow 'AccessDenied
               :> CanThrow 'ServiceNotFound
               :> CanThrow 'BadCredentials
               :> CanThrow 'InvalidServiceKey
               :> ZProvider
               :> "provider"
               :> "services"
               :> Capture "service-id" ServiceId
               :> "connection"
               :> ReqBody '[JSON] Public.UpdateServiceConn
               :> MultiVerb1 'PUT '[PlainText, JSON] (RespondEmpty 200 "Provider service connection updated")
           )
    :<|> Named
           "delete-provider-services-by-service-id"
           ( Summary "Delete service"
               :> CanThrow 'AccessDenied
               :> CanThrow 'BadCredentials
               :> CanThrow 'ServiceNotFound
               :> ZProvider
               :> "provider"
               :> "services"
               :> Capture "service-id" ServiceId
               :> ReqBody '[JSON] Public.DeleteService
               :> MultiVerb1 'DELETE '[PlainText] (RespondEmpty 202 "")
           )
    :<|> Named
           "get-provider-services-by-provider-id"
           ( Summary "Get provider services by provider id"
               :> CanThrow 'AccessDenied
               :> ZUser
               :> "providers"
               :> Capture "provider-id" ProviderId
               :> "services"
               :> Get '[JSON] [Public.ServiceProfile]
           )
    :<|> Named
           "get-services"
           ( Summary "List services"
               :> CanThrow 'AccessDenied
               :> ZUser
               :> "services"
               :> QueryParam "tags" (QueryAnyTags 1 3)
               :> QueryParam "start" Text
               :> QueryParam "size" (Range 10 100 Int32) -- Default to 20
               :> Get '[JSON] Public.ServiceProfilePage
           )
    :<|> Named
           "get-services-tags"
           ( Summary "Get services tags"
               :> CanThrow 'AccessDenied
               :> ZUser
               :> "services"
               :> "tags"
               :> Get '[JSON] ServiceTagList
           )
    :<|> Named
           "get-provider-services-by-provider-id-and-service-id"
           ( Summary "Get provider service by provider id and service id"
               :> CanThrow 'AccessDenied
               :> CanThrow 'ServiceNotFound
               :> ZUser
               :> "providers"
               :> Capture "provider-id" ProviderId
               :> "services"
               :> Capture "service-id" ServiceId
               :> Get '[JSON] Public.ServiceProfile
           )
    :<|> Named
           "get-whitelisted-services-by-team-id"
           ( Summary "Get whitelisted services by team id"
               :> ZUser
               :> "teams"
               :> Capture "team-id" TeamId
               :> "services"
               :> "whitelisted"
               :> QueryParam "prefix" (Range 1 128 Text)
               -- Default to True
               :> QueryParam "filter_disabled" Bool
               -- Default to 20
               :> QueryParam "size" (Range 10 100 Int32)
               :> Get '[JSON] Public.ServiceProfilePage
           )
    :<|> Named
           "post-team-whitelist-by-team-id"
           ( Summary "Update service whitelist"
               :> ZUser
               :> ZConn
               :> "teams"
               :> Capture "team-id" TeamId
               :> "services"
               :> "whitelist"
               :> ReqBody '[JSON] Public.UpdateServiceWhitelist
               :> MultiVerb
                    'POST
                    '[PlainText]
                    '[ RespondEmpty 200 "UpdateServiceWhitelistRespChanged",
                       RespondEmpty 204 "UpdateServiceWhitelistRespUnchanged"
                     ]
                    Public.UpdateServiceWhitelistResp
           )
