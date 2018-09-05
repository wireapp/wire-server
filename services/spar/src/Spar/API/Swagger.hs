{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Swagger where

import Data.Id
import Data.Proxy
import Data.UUID (UUID)
import Data.String.Conversions (cs)
import Data.String.Interpolate as QQ
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.
import Lens.Micro
import Servant
import Servant.Swagger
import Spar.API.Instances ()
import Spar.Types
import Web.Cookie (SetCookie)

import qualified Data.Swagger.SchemaOptions as Swagger
import qualified Data.X509 as X509
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Types.TH as SAML
import qualified Servant.Multipart as SM
import qualified URI.ByteString as URI


-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

instance ToSchema Swagger where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())
    & mapped . schema . description ?~ "The swagger docs you are looking at (all details hidden)."

instance HasSwagger route => HasSwagger (SM.MultipartForm SM.Mem resp :> route) where
  toSwagger _proxy = toSwagger (Proxy @route)
    & info . description ?~ cs [QQ.i|

# Overview

`/sso/metadata` will be requested by the IdPs to learn how to talk to wire.

`/sso/initiate-login/`, `/sso/finalize-login` are for the SAML authentication handshake performed by a user in order to log into wire.  They are not exactly standard in their details: they may return HTML or XML; redirect to error URLs instead of throwing errors, etc.

`/identity-providers` end-points are for use in the team settings page when IdPs are registered.  They talk json.


# Configuring IdPs

## okta.com

Okta will ask you to provide two URLs when you set it up for talking to wireapp:

1. The `Single sign on URL`.  This is the end-point that accepts the user's credentials after successful authentication against the IdP.  Choose `/sso/finalize-login` with schema and hostname of the wire server you are configuring.

2. The `Audience URI`.  You can find this in the metadata returned by the `/sso/metadata` end-point.  It is the contents of the `md:OrganizationURL` element.

## microsoft azure active directory

(coming up.)

|]

-- | The options to use for schema generation. Must match the options used
-- for 'ToJSON' instances elsewhere.
samlSchemaOptions :: SchemaOptions
samlSchemaOptions = Swagger.fromAesonOptions SAML.deriveJSONOptions

instance ToParamSchema TeamId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToParamSchema UserId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToParamSchema SAML.IdPId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema TeamId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)
instance ToSchema UserId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)
instance ToSchema SAML.IdPId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema SAML.AuthnRequest where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.FormRedirect SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions
  -- TODO: would be nice to add an example here, but that only works for json?

instance ToSchema SPInfo where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema IdPExtra where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema a => ToSchema (SAML.IdPConfig a) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema IdPList where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.NewIdP where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.ID SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Issuer where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SAML.Time where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Version where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema X509.SignedCertificate where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SAML.SPDesc where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema URI.URI where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToParamSchema URI.URI where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToParamSchema SetCookie where
  toParamSchema _ = toParamSchema (Proxy @Bool)
