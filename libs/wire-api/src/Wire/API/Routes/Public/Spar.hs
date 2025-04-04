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

import Control.Lens ((^.))
import Data.Domain
import Data.Id
import Data.Kind (Type)
import Data.Proxy
import Data.Range
import Imports
import SAML2.WebSSO qualified as SAML
import Servant
import Servant.API.Extended
import Servant.Multipart
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
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
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
  Named
    "sso-metadata"
    ( --  This deprecated endpoint should be removed at some point. However it does not make a lot of sense to apply our versioning mechanism to it,
      -- as this is not a classic client API endpoint. It is used in the SAML IDP flow and should exist independently of the API version,
      -- and requires a different process for decommissioning. See https://wearezeta.atlassian.net/browse/WPB-15319
      DeprecateSSOAPIV1 :> Deprecated :> "metadata" :> ZHostOpt :> SAML.APIMeta
    )
    :<|> Named "sso-team-metadata" ("metadata" :> ZHostOpt :> Capture "team" TeamId :> SAML.APIMeta)
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
        :> ZHostOpt
        :> Get '[SAML.HTML] (SAML.FormRedirect SAML.AuthnRequest)
    )

-- | This deprecated endpoint should be removed at some point. However it does not make a lot of sense to apply our versioning mechanism to it,
-- as this is not a classic client API endpoint. It is used in the SAML IDP flow and should exist independently of the API version,
-- and requires a different process for decommissioning. See https://wearezeta.atlassian.net/browse/WPB-15319
type APIAuthRespLegacy =
  Named
    "auth-resp-legacy"
    ( DeprecateSSOAPIV1
        :> Deprecated
        :> "finalize-login"
        -- (SAML.APIAuthResp from here on, except for response)
        :> MultipartForm Mem SAML.AuthnResponseBody
        :> ZHostOpt
        :> Post '[PlainText] Void
    )

type APIAuthResp =
  Named
    "auth-resp"
    ( "finalize-login"
        :> Capture "team" TeamId
        -- (SAML.APIAuthResp from here on, except for response)
        :> MultipartForm Mem SAML.AuthnResponseBody -- this is the *http request* body containing the *saml response*.
        :> ZHostOpt
        :> Post '[PlainText] Void
    )

type APIIDP =
  Named "idp-get" (ZOptUser :> IdpGet)
    :<|> Named "idp-get-raw" (ZOptUser :> IdpGetRaw)
    :<|> Named "idp-get-all" (ZOptUser :> IdpGetAll)
    :<|> Named "idp-create@v7" (Until 'V8 :> ZOptUser :> IdpCreate) -- (change is semantic, see handler)
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

sparSPIssuer :: (Functor m, SAML.HasConfig m) => Maybe TeamId -> Maybe Domain -> m (Maybe SAML.Issuer)
sparSPIssuer mbtid = (SAML.Issuer <$$>) . sparResponseURI mbtid

sparResponseURI :: (Functor m, SAML.HasConfig m) => Maybe TeamId -> Maybe Domain -> m (Maybe URI.URI)
sparResponseURI Nothing =
  getSsoURI (Proxy @APISSO) (Proxy @APIAuthRespLegacy)
sparResponseURI (Just tid) =
  getSsoURI' (Proxy @APISSO) (Proxy @APIAuthResp) tid

getSsoURI ::
  forall m endpoint api.
  ( HasCallStack,
    Functor m,
    SAML.HasConfig m,
    IsElem endpoint api,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint Link)
  ) =>
  Proxy api ->
  Proxy endpoint ->
  Maybe Domain ->
  m (Maybe URI.URI)
getSsoURI proxyAPI proxyAPIAuthResp mbDomain = (extpath . (^. SAML.cfgSPSsoURI)) <$$> (domainConfig <$> SAML.getConfig)
  where
    extpath :: URI.URI -> URI.URI
    extpath = (SAML.=/ (toUrlPiece $ safeLink proxyAPI proxyAPIAuthResp))

    domainConfig :: SAML.Config -> Maybe SAML.MultiIngressDomainConfig
    domainConfig config = SAML.getMultiIngressDomainConfig config mbDomain

-- | 'getSsoURI' for links that have one variable path segment.
--
-- FUTUREWORK: this is only sometimes what we need.  it would be nice to have a type class with a
-- method 'getSsoURI' for arbitrary path arities.
getSsoURI' ::
  forall endpoint api a (f :: Type -> Type) t.
  ( Functor f,
    SAML.HasConfig f,
    MkLink endpoint Link ~ (t -> a),
    HasLink endpoint,
    ToHttpApiData a,
    IsElem endpoint api
  ) =>
  Proxy api ->
  Proxy endpoint ->
  t ->
  Maybe Domain ->
  f (Maybe URI.URI)
getSsoURI' proxyAPI proxyAPIAuthResp idpid mbDomain = (extpath . (^. SAML.cfgSPSsoURI)) <$$> (domainConfig <$> SAML.getConfig)
  where
    extpath :: URI.URI -> URI.URI
    extpath = (SAML.=/ (toUrlPiece $ safeLink proxyAPI proxyAPIAuthResp idpid))

    domainConfig :: SAML.Config -> Maybe SAML.MultiIngressDomainConfig
    domainConfig config = SAML.getMultiIngressDomainConfig config mbDomain

getContactPersons ::
  ( Functor f,
    SAML.HasConfig f
  ) =>
  Maybe Domain ->
  f [SAML.ContactPerson]
getContactPersons mbDomain = domainConfig <$> SAML.getConfig
  where
    domainConfig :: SAML.Config -> [SAML.ContactPerson]
    domainConfig config = concatMap SAML._cfgContacts (SAML.getMultiIngressDomainConfig config mbDomain)

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
  Named "auth-tokens-create@v7" (Until 'V8 :> ZOptUser :> APIScimTokenCreateV7)
    :<|> Named "auth-tokens-create" (From 'V8 :> ZOptUser :> APIScimTokenCreate)
    :<|> Named "auth-tokens-put-name" (From 'V8 :> ZUser :> APIScimTokenPutName)
    :<|> Named "auth-tokens-delete" (ZOptUser :> APIScimTokenDelete)
    :<|> Named "auth-tokens-list@v7" (Until 'V8 :> ZOptUser :> APIScimTokenListV7)
    :<|> Named "auth-tokens-list" (From 'V8 :> ZOptUser :> APIScimTokenList)

type APIScimTokenPutName =
  Capture "id" ScimTokenId
    :> ReqBody '[JSON] ScimTokenName
    :> Put '[JSON] ()

type APIScimTokenCreateV7 =
  VersionedReqBody 'V7 '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponseV7

type APIScimTokenCreate =
  ReqBody '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponse

type APIScimTokenDelete =
  QueryParam' '[Required, Strict] "id" ScimTokenId
    :> DeleteNoContent

type APIScimTokenList =
  Get '[JSON] ScimTokenList

type APIScimTokenListV7 =
  Get '[JSON] ScimTokenListV7

data SparAPITag

instance ServiceAPI SparAPITag v where
  type ServiceAPIRoutes SparAPITag = SparAPI
