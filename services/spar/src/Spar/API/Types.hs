{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.API.Types where

import Imports
import Data.Id
import Data.Proxy
import Data.String.Conversions (ST)
import Servant
import Servant.Multipart
import Spar.Types

import qualified SAML2.WebSSO as SAML
import qualified URI.ByteString as URI

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.


-- FUTUREWORK (thanks jschaul): Use @Header' '[Strict]@ to avoid the need for the 'Maybe' and the
-- extra checks.

-- FUTUREWORK: use https://hackage.haskell.org/package/servant-0.14.1/docs/Servant-API-Generic.html?

type API
     = "sso" :> APISSO
  :<|> "sso-initiate-bind"  :> APIAuthReq  -- (see comment on 'APIAuthReq')
  :<|> "identity-providers" :> APIIDP
  -- TODO: reenable once SCIM is ready
  -- :<|> "scim" :> SCIM.API
  :<|> "i" :> APIINTERNAL
  -- NB. If you add endpoints here, also update Test.Spar.APISpec

type APISSO
     = "api-docs" :> Get '[JSON] Swagger
  :<|> APIMeta
  :<|> "initiate-login" :> APIAuthReqPrecheck
  :<|> "initiate-login" :> APIAuthReq
  :<|> APIAuthResp

type CheckOK = Verb 'HEAD 200

type APIMeta
     = "metadata" :> SAML.APIMeta

type APIAuthReqPrecheck
     = QueryParam "success_redirect" URI.URI
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
-- Why a special cookie, and not the session cookie, or the session token?  The wire session cookie
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
type APIAuthReq
     = Header "Z-User" UserId
    :> QueryParam "success_redirect" URI.URI
    :> QueryParam "error_redirect" URI.URI
       -- (SAML.APIAuthReq from here on, except for the cookies)
    :> Capture "idp" SAML.IdPId
    :> Get '[SAML.HTML] (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))

data DoInitiate = DoInitiateLogin | DoInitiateBind
  deriving (Eq, Show, Bounded, Enum)

type WithSetBindCookie = Headers '[Servant.Header "Set-Cookie" SetBindCookie]

type APIAuthResp
     = "finalize-login"
    :> Header "Cookie" ST
       -- (SAML.APIAuthResp from here on, except for response)
    :> MultipartForm Mem SAML.AuthnResponseBody
    :> Post '[PlainText] Void

type APIIDP
     = Header "Z-User" UserId :> IdpGet
  :<|> Header "Z-User" UserId :> IdpGetAll
  :<|> Header "Z-User" UserId :> IdpCreate
  :<|> Header "Z-User" UserId :> IdpDelete

type IdpGet     = Capture "id" SAML.IdPId :> Get '[JSON] IdP
type IdpGetAll  = Get '[JSON] IdPList
type IdpCreate  = ReqBody '[SAML.XML] SAML.IdPMetadata :> PostCreated '[JSON] IdP
type IdpDelete  = Capture "id" SAML.IdPId :> DeleteNoContent '[JSON] NoContent

type APIINTERNAL
     = "status" :> Get '[JSON] NoContent


sparSPIssuer :: SAML.HasConfig m => m SAML.Issuer
sparSPIssuer = SAML.Issuer <$> SAML.getSsoURI (Proxy @APISSO) (Proxy @APIAuthResp)

sparResponseURI :: SAML.HasConfig m => m URI.URI
sparResponseURI = SAML.getSsoURI (Proxy @APISSO) (Proxy @APIAuthResp)


-- | Type families to convert spar's 'API' type into an "outside-world-view" API type
-- to expose as swagger docs intended to be used by client developers.
-- Here we assume the 'spar' service is only accessible from behind the 'nginz' proxy, which
--   * does not expose routes prefixed with /i/
--   * handles authorization (adding a Z-User header if requests are authorized)
--   * does not show the swagger end-point itself
type OutsideWorldAPI =
    StripSwagger (StripInternal (StripAuth API))

-- | Strip the nginz-set, internal-only Z-User header
type family StripAuth api where
    StripAuth (Header "Z-User" a :> b) = b
    StripAuth (a :<|> b) = StripAuth a :<|> StripAuth b
    StripAuth x = x

-- | Strip internal endpoints (prefixed with /i/)
type family StripInternal api where
    StripInternal ("i" :> b) = EmptyAPI
    StripInternal (a :<|> b) = StripInternal a :<|> StripInternal b
    StripInternal x = x

-- | Strip the endpoint that exposes documentation.
type family StripSwagger api where
    StripSwagger ("sso" :> "api-docs" :> a) = EmptyAPI
    StripSwagger (a :<|> b) = StripSwagger a :<|> StripSwagger b
    StripSwagger x = x
