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

module Wire.API.Routes.Public.Brig.Provider where

import Data.Code qualified as Code
import Imports
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Error (CanThrow)
import Wire.API.Error.Brig
import Wire.API.Provider
import Wire.API.Routes.API (ServiceAPI (..))
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named (..))
import Wire.API.User.Auth

type ActivateResponses =
  '[ RespondEmpty 204 "",
     Respond 200 "" ProviderActivationResponse
   ]

type ProviderAPI =
  Named
    "provider-register"
    ( Summary "Register a new provider"
        :> CanThrow 'AccessDenied
        :> CanThrow 'InvalidEmail
        :> CanThrow 'VerificationCodeThrottled
        :> "provider"
        :> "register"
        :> ReqBody '[JSON] NewProvider
        :> MultiVerb1 'POST '[JSON] (Respond 201 "" NewProviderResponse)
    )
    :<|> Named
           "provider-activate"
           ( Summary "Activate a provider"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidCode
               :> "provider"
               :> "activate"
               :> QueryParam' '[Required, Strict] "key" Code.Key
               :> QueryParam' '[Required, Strict] "code" Code.Value
               :> MultiVerb 'GET '[JSON] ActivateResponses (Maybe ProviderActivationResponse)
           )
    :<|> Named
           "provider-login"
           ( Summary "Login as a provider"
               :> CanThrow 'AccessDenied
               :> CanThrow 'BadCredentials
               :> "provider"
               :> "login"
               :> ReqBody '[JSON] ProviderLogin
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[ WithHeaders
                         '[ Header "Set-Cookie" ProviderTokenCookie
                          ]
                         ProviderTokenCookie
                         (RespondEmpty 200 "OK")
                     ]
                    ProviderTokenCookie
           )

data ProviderAPITag

instance ServiceAPI ProviderAPITag v where
  type ServiceAPIRoutes ProviderAPITag = ProviderAPI
