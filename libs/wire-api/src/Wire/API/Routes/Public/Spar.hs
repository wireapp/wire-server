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

module Wire.API.Routes.Public.Spar where

import Data.Id
import Data.Proxy
import Data.String.Conversions (ST)
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
import Wire.API.Cookie
import Wire.API.Routes.Public
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim

-- FUTUREWORK (thanks jschaul): Use @Header' '[Strict]@ to avoid the need for the 'Maybe' and the
-- extra checks.

-- FUTUREWORK: use https://hackage.haskell.org/package/servant-0.14.1/docs/Servant-API-Generic.html?

type API =
  "sso" :> APISSO
    :<|> "sso-initiate-bind" :> APIAuthReqPrecheck -- (see comment on 'APIAuthReq')
    :<|> "sso-initiate-bind" :> APIAuthReq -- (see comment on 'APIAuthReq')
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

-- | Dual-use route for initiating either login or bind.
--
-- We could distinguish the two cases by the presence or absence of a `Z-User` header.  However, we
-- also need to use this route under two different prefices because nginz only supports mandatory
-- auth and no auth, but not optional auth for any given end-point.  See also: 'DoInitiate'.
--
-- __Binding existing users to SSO__
--
-- A user has an existing account with email address and password, and gets idp credentails.  She
-- now wants to upgrade her password-based login to sso login, or *bind* the existing user to the
-- sso credentials.
--
-- __The Solution__
--
--   0. user logs in with password
--   1. inside the session, she requests `/sso-initiate-bind/<idp>` for an idp of her choice (idp
--      must be registered with her team)
--   2. spar checks the `Z-User` header and sends a short-lived bind cookie with the response that
--      otherwise is the same as for `/sso/initiate-login/<idp>`
--   3. everybody goes through the well-known SAML web sso moves, until:
--   4. the authentication response is sent to `/sso/finalize-login/<idp>` together with the bind
--      cookie
--   5. spar identifies the user both via the bind cookie and via the SAML authentication response,
--      and performs the binding.
--
-- Why a special cookie, and not the cookie already available, or the session token?  The wire cookie
-- gets only sent to `/access`, and we would need to change that; session tokens are hard to handle
-- while switching between app context and browser context.  Having a separate cookie makes both the
-- context switching simple and allows us to set a different cookie recipient end-point and short
-- cookie lifetime.
--
-- This solution is very flexible.  UX variants:
--
--   * team admin posts the initiate-bind link in a group in the wire team.  no new UI components
--     are needed in the frontend.
--   * we add buttons in the settings page somewhere.
--   * we send a link containing a token to the user by email.  when the user clicks on the link,
--     she gets authenticated and redirected to the initiate-bind end-point in step 1 above.
--   * ...?
--
-- __Corner Case: Accidental Creation of new SSO User__
--
-- What happens if the user authenticates via SSO first, creates a new user, and then receives the
-- bind invite?
--
-- Possible solutions:
--
--   * The IdP could create all users via SCIM and we block implicit user creation.
--   * After the fact, the duplicated user deletes the SSO user, loses a little bit of user data,
--     and goes through the bind process.
--   * Block implicit creation for a short time window, and ask all existing users to use that time
--     window to bind.
type APIAuthReq =
  ZOptUser
    :> QueryParam "success_redirect" URI.URI
    :> QueryParam "error_redirect" URI.URI
    -- (SAML.APIAuthReq from here on, except for the cookies)
    :> Capture "idp" SAML.IdPId
    :> Get '[SAML.HTML] (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))

data DoInitiate = DoInitiateLogin | DoInitiateBind
  deriving (Eq, Show, Bounded, Enum)

type WithSetBindCookie = Headers '[Servant.Header "Set-Cookie" SetBindCookie]

type APIAuthRespLegacy =
  "finalize-login"
    :> Header "Cookie" ST
    -- (SAML.APIAuthResp from here on, except for response)
    :> MultipartForm Mem SAML.AuthnResponseBody
    :> Post '[PlainText] Void

type APIAuthResp =
  "finalize-login"
    :> Capture "team" TeamId
    :> Header "Cookie" ST
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
    :<|> "auth-tokens" :> APIScimToken

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
