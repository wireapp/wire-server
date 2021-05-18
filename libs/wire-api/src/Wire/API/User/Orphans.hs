{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
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

module Wire.API.User.Orphans where

import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Proxy
import Data.Swagger
import Data.UUID
import Data.X509 as X509
import Imports
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Servant.API ((:>))
import qualified Servant.Multipart as SM
import Servant.Swagger
import URI.ByteString

deriving instance Generic ISO639_1

-- Swagger instances

instance ToSchema ISO639_1

instance ToSchema CountryCode

-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart
-- FUTUREWORK: maybe avoid orphans altogether by defining schema instances manually

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart

-- | The options to use for schema generation. Must match the options used
-- for 'ToJSON' instances elsewhere.
samlSchemaOptions :: SchemaOptions
samlSchemaOptions = fromAesonOptions deriveJSONOptions

instance ToSchema SAML.XmlText where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToParamSchema SAML.IdPId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema SAML.AuthnRequest where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.NameIdPolicy where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.NameIDFormat where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.FormRedirect SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.ID SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Time where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.SPMetadata where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema Void where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance HasSwagger route => HasSwagger (SM.MultipartForm SM.Mem resp :> route) where
  toSwagger _proxy = toSwagger (Proxy @route)

instance ToSchema SAML.IdPId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema SAML.IdPMetadata where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema a => ToSchema (SAML.IdPConfig a) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Issuer where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema URI where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToParamSchema URI where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToSchema X509.SignedCertificate where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)
