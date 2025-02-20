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
import Data.Id (ProviderId)
import Data.Misc
import Imports
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Provider
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named)
import Wire.API.Routes.Public
import Wire.API.User.Auth

type ActivateResponses =
  '[ RespondEmpty 204 "",
     Respond 200 "" ProviderActivationResponse
   ]

type GetProviderResponses =
  '[ ErrorResponse 'ProviderNotFound,
     Respond 200 "" Provider
   ]

type GetProviderProfileResponses =
  '[ ErrorResponse 'ProviderNotFound,
     Respond 200 "" ProviderProfile
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
        :> Header' '[Required, Strict] "X-Forwarded-For" IpAddr
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
    :<|> Named
           "provider-password-reset"
           ( Summary "Begin a password reset"
               :> CanThrow 'AccessDenied
               :> CanThrow 'BadCredentials
               :> CanThrow 'InvalidPasswordResetKey
               :> CanThrow 'InvalidPasswordResetCode
               :> CanThrow 'PasswordResetInProgress
               :> CanThrow 'PasswordResetInProgress
               :> CanThrow 'ResetPasswordMustDiffer
               :> CanThrow 'VerificationCodeThrottled
               :> "provider"
               :> "password-reset"
               :> ReqBody '[JSON] PasswordReset
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 201 "")
           )
    :<|> Named
           "provider-password-reset-complete"
           ( Summary "Complete a password reset"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidCode
               :> CanThrow 'ResetPasswordMustDiffer
               :> CanThrow 'BadCredentials
               :> CanThrow 'InvalidPasswordResetCode
               :> "provider"
               :> "password-reset"
               :> "complete"
               :> ReqBody '[JSON] CompletePasswordReset
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "")
           )
    :<|> Named
           "provider-delete"
           ( Summary "Delete a provider"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidProvider
               :> CanThrow 'BadCredentials
               :> ZProvider
               :> "provider"
               :> ReqBody '[JSON] DeleteProvider
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "")
           )
    :<|> Named
           "provider-update"
           ( Summary "Update a provider"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidProvider
               :> ZProvider
               :> "provider"
               :> ReqBody '[JSON] UpdateProvider
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "")
           )
    :<|> Named
           "provider-update-email"
           ( Summary "Update a provider email"
               :> CanThrow 'AccessDenied
               :> CanThrow 'InvalidEmail
               :> CanThrow 'InvalidProvider
               :> CanThrow 'VerificationCodeThrottled
               :> ZProvider
               :> "provider"
               :> "email"
               :> ReqBody '[JSON] EmailUpdate
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 202 "")
           )
    :<|> Named
           "provider-update-password"
           ( Summary "Update a provider password"
               :> CanThrow 'AccessDenied
               :> CanThrow 'BadCredentials
               :> CanThrow 'ResetPasswordMustDiffer
               :> ZProvider
               :> "provider"
               :> "password"
               :> ReqBody '[JSON] PasswordChange
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "")
           )
    :<|> Named
           "provider-get-account"
           ( Summary "Get account"
               :> CanThrow 'AccessDenied
               :> CanThrow 'ProviderNotFound
               :> ZProvider
               :> "provider"
               :> MultiVerb 'GET '[JSON] GetProviderResponses (Maybe Provider)
           )
    :<|> Named
           "provider-get-profile"
           ( Summary "Get profile"
               :> ZUser
               :> "providers"
               :> Capture "pid" ProviderId
               :> MultiVerb 'GET '[JSON] GetProviderProfileResponses (Maybe ProviderProfile)
           )
