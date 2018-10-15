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

import Data.Id
import Data.Proxy
import Data.String.Conversions
import Servant
import Servant.Multipart
import Spar.API.Test
import Spar.Types

import qualified SAML2.WebSSO as SAML
import qualified Spar.SCIM as SCIM
import qualified URI.ByteString as URI

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.


-- FUTUREWORK (thanks jschaul): Use @Header' '[Strict]@ to avoid the need for the 'Maybe' and the
-- extra checks.

-- FUTUREWORK: use servant-generic?

type API
     = "sso" :> APISSO
  :<|> "identity-providers" :> APIIDP
  :<|> "scim" :> SCIM.API
  :<|> "i" :> APIINTERNAL
  -- NB. If you add endpoints here, also update Test.Spar.APISpec

type APISSO
     = "api-docs" :> Get '[JSON] Swagger
  :<|> APIMeta
  :<|> "initiate-login" :> APIAuthReqPrecheck
  :<|> "initiate-login" :> APIAuthReq
  :<|> "initiate-bind"  :> APIAuthReq          -- (see comment on 'APIAuthReq')
  :<|> APIAuthResp

type CheckOK = Verb 'HEAD 200

type APIMeta
     = "metadata" :> SAML.APIMeta

type APIAuthReqPrecheck
     = QueryParam "success_redirect" URI.URI
    :> QueryParam "error_redirect" URI.URI
    :> Capture "idp" SAML.IdPId
    :> CheckOK '[PlainText] NoContent

-- | The route for bind and login, without the prefix.  We need to use two different prefices
-- because one requires unauthenticated requests, and one requires authenticated requests.  nginz
-- only supports mandatory and no, but not optional authentication for any given end-point.  See
-- also: 'DoInitiate'.)
type APIAuthReq
     = Header "Z-User" UserId
    :> QueryParam "success_redirect" URI.URI
    :> QueryParam "error_redirect" URI.URI
       -- (SAML.APIAuthReq from here on, except for the cookies)
    :> Capture "idp" SAML.IdPId
    :> Get '[SAML.HTML] (WithBindCookie (SAML.FormRedirect SAML.AuthnRequest))

data DoInitiate = DoInitiateLogin | DoInitiateBind
  deriving (Eq, Show, Bounded, Enum)

doInitiatePath :: DoInitiate -> ST
doInitiatePath DoInitiateLogin = "initiate-login"
doInitiatePath DoInitiateBind  = "initiate-bind"

type WithBindCookie = Headers '[Servant.Header "Set-Cookie" BindCookie]

type APIAuthResp
     = "finalize-login"
    :> Header "Cookie" BindCookie
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
  :<|> "integration-tests" :> IntegrationTests


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
--
-- In addition, we strip the SCIM documentation because SCIM doesn't have
-- Swagger schemas yet.
type OutsideWorldAPI =
    StripSwagger (StripSCIM (StripInternal (StripAuth API)))

-- | Strip the nginz-set, internal-only Z-User header
type family StripAuth api where
    StripAuth (Header "Z-User" a :> b) = b
    StripAuth (a :<|> b) = (StripAuth a) :<|> (StripAuth b)
    StripAuth x = x

-- | Strip internal endpoints (prefixed with /i/)
type family StripInternal api where
    StripInternal ("i" :> b) = EmptyAPI
    StripInternal (a :<|> b) = (StripInternal a) :<|> (StripInternal b)
    StripInternal x = x

-- | Strip the endpoint that exposes documentation.
type family StripSwagger api where
    StripSwagger ("sso" :> "api-docs" :> Get '[JSON] Swagger :<|> b) = EmptyAPI
    StripSwagger (a :<|> b) = StripSwagger a :<|> StripSwagger b
    StripSwagger x = x

-- | Strip SCIM.
type family StripSCIM api where
    StripSCIM ("scim" :> a) = EmptyAPI
    StripSCIM (a :<|> b) = StripSCIM a :<|> StripSCIM b
    StripSCIM x = x
