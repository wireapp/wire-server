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

module Wire.API.Routes.Public.Brig.OAuth where

import Data.Id as Id
import Data.SOP
import Imports hiding (exp, head)
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.OAuth
import Wire.API.Password
import Wire.API.Routes.API
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named)
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type OAuthAPI =
  Named
    "get-oauth-client"
    ( Summary "Get OAuth client information"
        :> CanThrow 'OAuthFeatureDisabled
        :> CanThrow 'OAuthClientNotFound
        :> ZUser
        :> "oauth"
        :> "clients"
        :> Capture' '[Description "The ID of the OAuth client"] "OAuthClientId" OAuthClientId
        :> MultiVerb
             'GET
             '[JSON]
             '[ ErrorResponse 'OAuthClientNotFound,
                Respond 200 "OAuth client found" OAuthClient
              ]
             (Maybe OAuthClient)
    )
    :<|> Named
           "create-oauth-auth-code"
           ( Summary "Create an OAuth authorization code"
               :> Description "Currently only supports the 'code' response type, which corresponds to the authorization code flow."
               :> ZUser
               :> "oauth"
               :> "authorization"
               :> "codes"
               :> ReqBody '[JSON] CreateOAuthAuthorizationCodeRequest
               :> MultiVerb
                    'POST
                    '[JSON]
                    CreateOAuthAuthorizationCodeResponses
                    CreateOAuthCodeResponse
           )
    :<|> Named
           "create-oauth-access-token"
           ( Summary "Create an OAuth access token"
               :> Description "Obtain a new access token from an authorization code or a refresh token."
               :> CanThrow 'OAuthJwtError
               :> CanThrow 'OAuthAuthorizationCodeNotFound
               :> CanThrow 'OAuthClientNotFound
               :> CanThrow 'OAuthFeatureDisabled
               :> CanThrow 'OAuthInvalidRefreshToken
               :> CanThrow 'OAuthInvalidGrantType
               :> CanThrow 'OAuthInvalidClientCredentials
               :> CanThrow 'OAuthInvalidGrant
               :> "oauth"
               :> "token"
               :> ReqBody '[FormUrlEncoded] (Either OAuthAccessTokenRequest OAuthRefreshAccessTokenRequest)
               :> Post '[JSON] OAuthAccessTokenResponse
           )
    :<|> Named
           "revoke-oauth-refresh-token"
           ( Summary "Revoke an OAuth refresh token"
               :> Description "Revoke an access token."
               :> CanThrow 'OAuthJwtError
               :> CanThrow 'OAuthInvalidRefreshToken
               :> CanThrow 'OAuthClientNotFound
               :> "oauth"
               :> "revoke"
               :> ReqBody '[JSON] OAuthRevokeRefreshTokenRequest
               :> Post '[JSON] ()
           )
    :<|> Named
           "get-oauth-applications"
           ( Summary "Get OAuth applications with account access"
               :> Description "Get all OAuth applications with active account access for a user."
               :> ZUser
               :> "oauth"
               :> "applications"
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "OAuth applications found" [OAuthApplication])
           )
    :<|> Named
           "revoke-oauth-account-access-v6"
           ( Summary "Revoke account access from an OAuth application"
               :> ZUser
               :> Until 'V7
               :> "oauth"
               :> "applications"
               :> Capture' '[Description "The ID of the OAuth client"] "OAuthClientId" OAuthClientId
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 204 "OAuth application access revoked"]
                    ()
           )
    :<|> Named
           "delete-oauth-refresh-token"
           ( Summary "Revoke an active OAuth session"
               :> Description "Revoke an active OAuth session by providing the refresh token ID."
               :> ZUser
               :> CanThrow 'AccessDenied
               :> CanThrow 'OAuthClientNotFound
               :> "oauth"
               :> "applications"
               :> Capture' '[Description "The ID of the OAuth client"] "OAuthClientId" OAuthClientId
               :> "sessions"
               :> Capture' '[Description "The ID of the refresh token"] "RefreshTokenId" OAuthRefreshTokenId
               :> ReqBody '[JSON] PasswordReqBody
               :> Delete '[JSON] ()
           )

type CreateOAuthAuthorizationCodeHeaders = '[Header "Location" RedirectUrl]

type CreateOAuthAuthorizationCodeResponses =
  '[ -- success
     WithHeaders CreateOAuthAuthorizationCodeHeaders RedirectUrl (RespondEmpty 201 "Created"),
     -- feature disabled
     WithHeaders CreateOAuthAuthorizationCodeHeaders RedirectUrl (RespondEmpty 403 "Forbidden"),
     -- unsupported response type
     WithHeaders CreateOAuthAuthorizationCodeHeaders RedirectUrl (RespondEmpty 400 "Bad Request"),
     -- client not found
     WithHeaders CreateOAuthAuthorizationCodeHeaders RedirectUrl (RespondEmpty 404 "Not Found"),
     -- redirect url mismatch
     ErrorResponse 'OAuthRedirectUrlMissMatch
   ]

data CreateOAuthCodeResponse
  = CreateOAuthCodeSuccess RedirectUrl
  | CreateOAuthCodeFeatureDisabled RedirectUrl
  | CreateOAuthCodeUnsupportedResponseType RedirectUrl
  | CreateOAuthCodeClientNotFound RedirectUrl
  | CreateOAuthCodeRedirectUrlMissMatch

instance AsUnion CreateOAuthAuthorizationCodeResponses CreateOAuthCodeResponse where
  toUnion :: CreateOAuthCodeResponse -> Union (ResponseTypes CreateOAuthAuthorizationCodeResponses)
  toUnion (CreateOAuthCodeSuccess url) = Z (I url)
  toUnion (CreateOAuthCodeFeatureDisabled url) = S (Z (I url))
  toUnion (CreateOAuthCodeUnsupportedResponseType url) = S (S (Z (I url)))
  toUnion (CreateOAuthCodeClientNotFound url) = S (S (S (Z (I url))))
  toUnion CreateOAuthCodeRedirectUrlMissMatch = S (S (S (S (Z (I (dynError @(MapError 'OAuthRedirectUrlMissMatch)))))))
  fromUnion :: Union (ResponseTypes CreateOAuthAuthorizationCodeResponses) -> CreateOAuthCodeResponse
  fromUnion (Z (I url)) = CreateOAuthCodeSuccess url
  fromUnion (S (Z (I url))) = CreateOAuthCodeFeatureDisabled url
  fromUnion (S (S (Z (I url)))) = CreateOAuthCodeUnsupportedResponseType url
  fromUnion (S (S (S (Z (I url))))) = CreateOAuthCodeClientNotFound url
  fromUnion (S (S (S (S (Z (I _)))))) = CreateOAuthCodeRedirectUrlMissMatch
  fromUnion (S (S (S (S (S x))))) = case x of {}

data OAuthAPITag

instance ServiceAPI OAuthAPITag v where
  type ServiceAPIRoutes OAuthAPITag = OAuthAPI
