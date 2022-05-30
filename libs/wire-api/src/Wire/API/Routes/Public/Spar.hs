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
import Data.Swagger (Swagger)
import Imports
import qualified SAML2.WebSSO as SAML
import Servant
import Servant.API.Extended
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Multipart
import Servant.Swagger (toSwagger)
import qualified URI.ByteString as URI
import Web.Scim.Capabilities.MetaSchema as Scim.Meta
import Web.Scim.Class.Auth as Scim.Auth
import Web.Scim.Class.User as Scim.User
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Routes.Public
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim

-- FUTUREWORK (thanks jschaul): Use @Header' '[Strict]@ to avoid the need for the 'Maybe' and the
-- extra checks.

-- FUTUREWORK: use https://hackage.haskell.org/package/servant-0.14.1/docs/Servant-API-Generic.html?

type API =
  "sso" :> APISSO
    :<|> "identity-providers" :> APIIDP
    :<|> "scim" :> APIScim
    :<|> OmitDocs :> "i" :> APIINTERNAL

type APISSO =
  "metadata" :> SAML.APIMeta
    :<|> "metadata" :> Capture "team" TeamId :> SAML.APIMeta
    :<|> "initiate-login" :> APIAuthReqPrecheck
    :<|> "initiate-login" :> APIAuthReq
    :<|> APIAuthRespLegacy
    :<|> APIAuthResp
    :<|> "settings" :> SsoSettingsGet

type CheckOK = Verb 'HEAD 200

type APIAuthReqPrecheck =
  QueryParam "success_redirect" URI.URI
    :> QueryParam "error_redirect" URI.URI
    :> Capture "idp" SAML.IdPId
    :> CheckOK '[PlainText] NoContent

type APIAuthReq =
  QueryParam "success_redirect" URI.URI
    :> QueryParam "error_redirect" URI.URI
    -- (SAML.APIAuthReq from here on, except for the cookies)
    :> Capture "idp" SAML.IdPId
    :> Get '[SAML.HTML] (SAML.FormRedirect SAML.AuthnRequest)

type APIAuthRespLegacy =
  "finalize-login"
    -- (SAML.APIAuthResp from here on, except for response)
    :> MultipartForm Mem SAML.AuthnResponseBody
    :> Post '[PlainText] Void

type APIAuthResp =
  "finalize-login"
    :> Capture "team" TeamId
    -- (SAML.APIAuthResp from here on, except for response)
    :> MultipartForm Mem SAML.AuthnResponseBody
    :> Post '[PlainText] Void

type APIIDP =
  ZOptUser :> IdpGet
    :<|> ZOptUser :> IdpGetRaw
    :<|> ZOptUser :> IdpGetAll
    :<|> ZOptUser :> IdpCreate
    :<|> ZOptUser :> IdpUpdate
    :<|> ZOptUser :> IdpDelete

type IdpGetRaw = Capture "id" SAML.IdPId :> "raw" :> Get '[RawXML] RawIdPMetadata

type IdpGet = Capture "id" SAML.IdPId :> Get '[JSON] IdP

type IdpGetAll = Get '[JSON] IdPList

-- | See also: 'validateNewIdP', 'idpCreate', 'idpCreateXML'.
type IdpCreate =
  ReqBodyCustomError '[RawXML, JSON] "wai-error" IdPMetadataInfo
    :> QueryParam' '[Optional, Strict] "replaces" SAML.IdPId
    :> QueryParam' '[Optional, Strict] "api_version" WireIdPAPIVersion
    :> PostCreated '[JSON] IdP

type IdpUpdate =
  ReqBodyCustomError '[RawXML, JSON] "wai-error" IdPMetadataInfo
    :> Capture "id" SAML.IdPId
    :> Put '[JSON] IdP

type IdpDelete =
  Capture "id" SAML.IdPId
    :> QueryParam' '[Optional, Strict] "purge" Bool
    :> DeleteNoContent

type SsoSettingsGet =
  Get '[JSON] SsoSettings

type APIINTERNAL =
  "status" :> Get '[JSON] NoContent
    :<|> "teams" :> Capture "team" TeamId :> DeleteNoContent
    :<|> "sso" :> "settings" :> ReqBody '[JSON] SsoSettings :> Put '[JSON] NoContent

sparSPIssuer :: SAML.HasConfig m => Maybe TeamId -> m SAML.Issuer
sparSPIssuer Nothing =
  SAML.Issuer <$> SAML.getSsoURI (Proxy @APISSO) (Proxy @APIAuthRespLegacy)
sparSPIssuer (Just tid) =
  SAML.Issuer <$> SAML.getSsoURI' (Proxy @APISSO) (Proxy @APIAuthResp) tid

sparResponseURI :: SAML.HasConfig m => Maybe TeamId -> m URI.URI
sparResponseURI Nothing =
  SAML.getSsoURI (Proxy @APISSO) (Proxy @APIAuthRespLegacy)
sparResponseURI (Just tid) =
  SAML.getSsoURI' (Proxy @APISSO) (Proxy @APIAuthResp) tid

-- SCIM

type APIScim =
  OmitDocs :> "v2" :> ScimSiteAPI SparTag
    :<|> "auth-tokens"
      :> CanThrow 'PasswordAuthenticationFailed
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
  ZOptUser :> APIScimTokenCreate
    :<|> ZOptUser :> APIScimTokenDelete
    :<|> ZOptUser :> APIScimTokenList

type APIScimTokenCreate =
  ReqBody '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponse

type APIScimTokenDelete =
  QueryParam' '[Required, Strict] "id" ScimTokenId
    :> DeleteNoContent

type APIScimTokenList =
  Get '[JSON] ScimTokenList

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy @API)
