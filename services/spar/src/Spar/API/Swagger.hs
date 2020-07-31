{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- | Swagger instances for Spar types (as well as some types that are used in Spar but not
-- defined in Spar).
module Spar.API.Swagger
  (
  )
where

import Control.Lens
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.Proxy
import Data.String.Conversions (cs)
import Data.String.Interpolate as QQ
import Data.Swagger hiding (Header (..))
import qualified Data.Swagger.SchemaOptions as Swagger
import Data.UUID (UUID)
import qualified Data.X509 as X509
import Imports
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Types.TH as SAML
import Servant
import qualified Servant.Multipart as SM
import Servant.Swagger
import Spar.Orphans ()
import Spar.Types
import qualified URI.ByteString as URI

-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

instance HasSwagger route => HasSwagger (SM.MultipartForm SM.Mem resp :> route) where
  toSwagger _proxy =
    toSwagger (Proxy @route)
      & info . description
        ?~ cs
          [QQ.i|

# Overview

`/sso/metadata` will be requested by the IdPs to learn how to talk to wire.

`/sso/initiate-login`, `/sso/finalize-login` are for the SAML authentication handshake performed by a user in order to log into wire.  They are not exactly standard in their details: they may return HTML or XML; redirect to error URLs instead of throwing errors, etc.

`/identity-providers` end-points are for use in the team settings page when IdPs are registered.  They talk json.


# Configuring IdPs

IdPs usually allow you to copy the metadata into your clipboard.  That should contain all the details you need to post the idp in your team under `/identity-providers`.  (Team id is derived from the authorization credentials of the request.)

## okta.com

Okta will ask you to provide two URLs when you set it up for talking to wireapp:

1. The `Single sign on URL`.  This is the end-point that accepts the user's credentials after successful authentication against the IdP.  Choose `/sso/finalize-login` with schema and hostname of the wire server you are configuring.

2. The `Audience URI`.  You can find this in the metadata returned by the `/sso/metadata` end-point.  It is the contents of the `md:OrganizationURL` element.

## centrify.com

Centrify allows you to upload the metadata xml document that you get from the `/sso/metadata` end-point.  You can also enter the metadata url and have centrify retrieve the xml, but to guarantee integrity of the setup, the metadata should be copied from the team settings page and pasted into the centrify setup page without any URL indirections.

## microsoft azure active directory

(coming up.)

|]

-- | The options to use for schema generation. Must match the options used
-- for 'ToJSON' instances elsewhere.
samlSchemaOptions :: SchemaOptions
samlSchemaOptions = Swagger.fromAesonOptions SAML.deriveJSONOptions

instance ToSchema SAML.XmlText where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToParamSchema (Id a) where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToParamSchema SAML.IdPId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema (Id a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema SAML.IdPId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema SAML.AuthnRequest where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.NameIdPolicy where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.NameIDFormat where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.FormRedirect SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

-- TODO: would be nice to add an example here, but that only works for json?

instance ToSchema a => ToSchema (SAML.IdPConfig a) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.IdPMetadata where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema IdPMetadataInfo where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "IdPMetadataInfo") $
        mempty
          & properties .~ properties_
          & minProperties ?~ 1
          & maxProperties ?~ 1
          & type_ .~ Just SwaggerObject
    where
      properties_ :: InsOrdHashMap Text (Referenced Schema)
      properties_ =
        fromList
          [ ("value", Inline (toSchema (Proxy @String)))
          ]

instance ToSchema IdPList where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema WireIdP where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.ID SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Issuer where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SAML.Time where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema X509.SignedCertificate where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SAML.SPMetadata where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema URI.URI where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToParamSchema URI.URI where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToParamSchema SetBindCookie where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToParamSchema BindCookie where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToSchema Void where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema RawIdPMetadata where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SsoSettings where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions
        { fieldLabelModifier = \case
            "defaultSsoCode" -> "default_sso_code"
            other -> other
        }
