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

module Wire.API.Routes.Public.Spar where

import Data.Id
import Data.Proxy
import Data.Range
import Imports
import SAML2.WebSSO qualified as SAML
import Servant
import Servant.API.Extended
import Servant.Multipart
import Servant.OpenApi
import URI.ByteString qualified as URI
import Web.Scim.Capabilities.MetaSchema as Scim.Meta
import Web.Scim.Class.Auth as Scim.Auth
import Web.Scim.Class.User as Scim.User
import Wire.API.Deprecated (Deprecated)
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Spar
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.SwaggerServant
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim

-- FUTUREWORK (thanks jschaul): Use @Header' '[Strict]@ to avoid the need for the 'Maybe' and the
-- extra checks.

-- FUTUREWORK: use https://hackage.haskell.org/package/servant-0.14.1/docs/Servant-API-Generic.html?

type SparAPI =
  "sso" :> APISSO
    :<|> "identity-providers" :> APIIDP
    :<|> "scim" :> APIScim
    :<|> OmitDocs :> InternalAPI

type DeprecateSSOAPIV1 =
  Description
    "DEPRECATED!  use /sso/metadata/:tid instead!  \
    \Details: https://docs.wire.com/understand/single-sign-on/trouble-shooting.html#can-i-use-the-same-sso-login-code-for-multiple-teams"

type APISSO =
  Named "sso-metadata" (DeprecateSSOAPIV1 :> Deprecated :> "metadata" :> SAML.APIMeta)
    :<|> Named "sso-team-metadata" ("metadata" :> Capture "team" TeamId :> SAML.APIMeta)
    :<|> "initiate-login" :> APIAuthReqPrecheck
    :<|> "initiate-login" :> APIAuthReq
    :<|> APIAuthRespLegacy
    :<|> APIAuthResp
    :<|> "settings" :> SsoSettingsGet

type CheckOK = Verb 'HEAD 200

type APIAuthReqPrecheck =
  Named
    "auth-req-precheck"
    ( QueryParam "success_redirect" URI.URI
        :> QueryParam "error_redirect" URI.URI
        :> Capture "idp" SAML.IdPId
        :> CheckOK '[PlainText] NoContent
    )

type APIAuthReq =
  Named
    "auth-req"
    ( QueryParam "success_redirect" URI.URI
        :> QueryParam "error_redirect" URI.URI
        -- (SAML.APIAuthReq from here on, except for the cookies)
        :> Capture "idp" SAML.IdPId
        :> Get '[SAML.HTML] (SAML.FormRedirect SAML.AuthnRequest)
    )

type APIAuthRespLegacy =
  Named
    "auth-resp-legacy"
    ( DeprecateSSOAPIV1
        :> Deprecated
        :> "finalize-login"
        -- (SAML.APIAuthResp from here on, except for response)
        :> MultipartForm Mem SAML.AuthnResponseBody
        :> Post '[PlainText] Void
    )

type APIAuthResp =
  Named
    "auth-resp"
    ( "finalize-login"
        :> Capture "team" TeamId
        -- (SAML.APIAuthResp from here on, except for response)
        :> MultipartForm Mem SAML.AuthnResponseBody
        :> Post '[PlainText] Void
    )

type APIIDP =
  Named "idp-get" (ZOptUser :> IdpGet)
    :<|> Named "idp-get-raw" (ZOptUser :> IdpGetRaw)
    :<|> Named "idp-get-all" (ZOptUser :> IdpGetAll)
    :<|> Named "idp-create" (ZOptUser :> IdpCreate)
    :<|> Named "idp-update" (ZOptUser :> IdpUpdate)
    :<|> Named "idp-delete" (ZOptUser :> IdpDelete)

type IdpGetRaw = Capture "id" SAML.IdPId :> "raw" :> Get '[RawXML] RawIdPMetadata

type IdpGet = Capture "id" SAML.IdPId :> Get '[JSON] IdP

type IdpGetAll = Get '[JSON] IdPList

-- | See also: 'validateNewIdP', 'idpCreate', 'idpCreateXML'.
type IdpCreate =
  ReqBodyCustomError '[RawXML, JSON] "wai-error" IdPMetadataInfo
    :> QueryParam' '[Optional, Strict] "replaces" SAML.IdPId
    :> QueryParam' '[Optional, Strict] "api_version" WireIdPAPIVersion -- see also: 'DeprecateSSOAPIV1'
    -- FUTUREWORK: The handle is restricted to 32 characters. Can we find a more reasonable upper bound and create a type for it? Also see `IdpUpdate`.
    :> QueryParam' '[Optional, Strict] "handle" (Range 1 32 Text)
    :> PostCreated '[JSON] IdP

type IdpUpdate =
  ReqBodyCustomError '[RawXML, JSON] "wai-error" IdPMetadataInfo
    :> Capture "id" SAML.IdPId
    -- FUTUREWORK: The handle is restricted to 32 characters. Can we find a more reasonable upper bound and create a type for it? Also see `IdpCreate`.
    :> QueryParam' '[Optional, Strict] "handle" (Range 1 32 Text)
    :> Put '[JSON] IdP

type IdpDelete =
  Capture "id" SAML.IdPId
    :> QueryParam' '[Optional, Strict] "purge" Bool
    :> DeleteNoContent

type SsoSettingsGet =
  Named
    "sso-settings"
    ( Get '[JSON] SsoSettings
    )

sparSPIssuer :: (Functor m, SAML.HasConfig m) => Maybe TeamId -> m SAML.Issuer
sparSPIssuer Nothing =
  SAML.Issuer <$> SAML.getSsoURI (Proxy @APISSO) (Proxy @APIAuthRespLegacy)
sparSPIssuer (Just tid) =
  SAML.Issuer <$> SAML.getSsoURI' (Proxy @APISSO) (Proxy @APIAuthResp) tid

sparResponseURI :: (Functor m, SAML.HasConfig m) => Maybe TeamId -> m URI.URI
sparResponseURI Nothing =
  SAML.getSsoURI (Proxy @APISSO) (Proxy @APIAuthRespLegacy)
sparResponseURI (Just tid) =
  SAML.getSsoURI' (Proxy @APISSO) (Proxy @APIAuthResp) tid

-- SCIM

type APIScim =
  OmitDocs :> "v2" :> ScimSiteAPI SparTag
    :<|> "auth-tokens"
      :> CanThrow 'CodeAuthenticationFailed
      :> CanThrow 'CodeAuthenticationRequired
      :> APIScimToken

type ScimSiteAPI tag = ToServantApi (ScimSite tag)

-- | This is similar to 'Scim.Site', but does not include the 'Scim.GroupAPI',
-- as we don't support it (we don't implement 'Web.Scim.Class.Group.GroupDB').
data ScimSite tag route = ScimSite
  { config ::
      route
        :- ToServantApi Scim.Meta.ConfigSite,
    users ::
      route
        :- Header "Authorization" (Scim.Auth.AuthData tag)
          :> "Users"
          :> ToServantApi (Scim.User.UserSite tag)
  }
  deriving (Generic)

type APIScimToken =
  Named "auth-tokens-create" (ZOptUser :> APIScimTokenCreate)
    :<|> Named "auth-tokens-delete" (ZOptUser :> APIScimTokenDelete)
    :<|> Named "auth-tokens-list" (ZOptUser :> APIScimTokenList)

type APIScimTokenCreate =
  ReqBody '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponse

type APIScimTokenDelete =
  QueryParam' '[Required, Strict] "id" ScimTokenId
    :> DeleteNoContent

type APIScimTokenList =
  Get '[JSON] ScimTokenList

data SparAPITag

instance ServiceAPI SparAPITag v where
  type ServiceAPIRoutes SparAPITag = SparAPI
  type SpecialisedAPIRoutes v SparAPITag = SparAPI
  serviceSwagger = toOpenApi (Proxy @SparAPI)
