{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.User.Orphans where

import Control.Lens
import Data.Aeson qualified as A
import Data.Char
import Data.Currency qualified as Currency
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.OpenApi as O
import Data.Proxy
import Data.Schema as S
import Data.Text qualified as T
import Data.UUID
import Data.X509 as X509
import Imports
import SAML2.WebSSO qualified as SAML
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Servant.API ((:>))
import Servant.Multipart qualified as SM
import Servant.OpenApi
import URI.ByteString

deriving instance Generic ISO639_1

-- Swagger instances

instance O.ToSchema ISO639_1

instance O.ToSchema CountryCode

-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart
-- FUTUREWORK: maybe avoid orphans altogether by defining schema instances manually

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

-- | The options to use for schema generation. Must match the options used
-- for 'ToJSON' instances elsewhere.
--
-- FUTUREWORK: This should be removed once the saml2-web-sso types are updated to remove their prefixes.
-- FUTUREWORK: Ticket for these changes https://wearezeta.atlassian.net/browse/WPB-3972
-- Preserve the old prefix semantics for types that are coming from outside of this repo.
samlSchemaOptions :: SchemaOptions
samlSchemaOptions =
  fromAesonOptions $
    deriveJSONOptions
      { A.fieldLabelModifier = fieldMod . dropPrefix
      }
  where
    fieldMod = A.fieldLabelModifier deriveJSONOptions
    dropPrefix = dropWhile (not . isUpper)

-- This type comes from a seperate repo, so we're keeping the prefix dropping
-- for the moment.
instance O.ToSchema SAML.XmlText where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToParamSchema SAML.IdPId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance O.ToSchema SAML.AuthnRequest where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance O.ToSchema SAML.NameIdPolicy where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance O.ToSchema SAML.NameIDFormat where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

-- The generic schema breaks on this type, so we define it by hand.
--
-- The reason is that genericDeclareNamedSchema tries to define the schema for
-- this type as a heterogeneous array (i.e. tuple) with Swagger types String
-- and AuthnRequest. However, Swagger does not support heterogeneous arrays,
-- and this results in an array whose underlying type which is at the same time
-- marked as a string, and referring to the schema for AuthnRequest, which is of
-- course invalid.
instance O.ToSchema (SAML.FormRedirect SAML.AuthnRequest) where
  declareNamedSchema _ = do
    authnReqSchema <- declareSchemaRef (Proxy @SAML.AuthnRequest)
    pure $
      NamedSchema (Just "FormRedirect") $
        mempty
          & type_ ?~ OpenApiObject
          & properties . at "uri" ?~ Inline (toSchema (Proxy @Text))
          & properties . at "xml" ?~ authnReqSchema

instance O.ToSchema (SAML.ID SAML.AuthnRequest) where
  declareNamedSchema =
    genericDeclareNamedSchema
      samlSchemaOptions
        { datatypeNameModifier = const "Id_AuthnRequest"
        }

instance O.ToSchema SAML.Time where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance O.ToSchema SAML.SPMetadata where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance O.ToSchema Void where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance (HasOpenApi route) => HasOpenApi (SM.MultipartForm SM.Mem resp :> route) where
  toOpenApi _proxy = toOpenApi (Proxy @route)

instance O.ToSchema SAML.IdPId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance (O.ToSchema a) => O.ToSchema (SAML.IdPConfig a) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance O.ToSchema SAML.Issuer where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance O.ToSchema URI where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance O.ToParamSchema URI where
  toParamSchema _ = toParamSchema (Proxy @String)

instance O.ToSchema X509.SignedCertificate where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance O.ToSchema SAML.IdPMetadata where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance S.ToSchema Currency.Alpha where
  schema = S.enum @Text "Currency.Alpha" cases & S.doc' . O.schema %~ swaggerTweaks
    where
      cases :: SchemaP [A.Value] Text (Alt Maybe Text) Currency.Alpha Currency.Alpha
      cases = mconcat ((\cur -> S.element (T.pack (show cur)) cur) <$> [minBound @Currency.Alpha ..])

      swaggerTweaks :: O.Schema -> O.Schema
      swaggerTweaks =
        ( O.description
            ?~ "ISO 4217 alphabetic codes. This is only stored by the backend, not processed. \
               \It can be removed once billing supports currency changes after team creation."
        )
          . (O.example ?~ "EUR")

deriving via (S.Schema Currency.Alpha) instance O.ToSchema Currency.Alpha
