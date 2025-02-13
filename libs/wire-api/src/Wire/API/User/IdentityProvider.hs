{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# LANGUAGE TemplateHaskell #-}

module Wire.API.User.IdentityProvider where

import Cassandra qualified as Cql
import Control.Lens (makeLenses, (.~), (?~))
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.ByteString qualified as AP
import Data.Binary.Builder qualified as BSB
import Data.ByteString.Conversion qualified as BSC
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Id (TeamId)
import Data.OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Media ((//))
import SAML2.WebSSO (IdPConfig)
import SAML2.WebSSO qualified as SAML
import SAML2.WebSSO.Test.Arbitrary ()
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Servant.API as Servant hiding (MkLink, URI (..))
import Wire.API.User.Orphans (samlSchemaOptions)
import Wire.API.Util.Aeson (defaultOptsDropChar)
import Wire.Arbitrary (Arbitrary, GenericUniform (GenericUniform))

-- | The identity provider type used in Spar.
type IdP = IdPConfig WireIdP

-- | Unique human-readable IdP name.
newtype IdPHandle = IdPHandle {unIdPHandle :: Text}
  deriving (Eq, Ord, Show, FromJSON, ToJSON, ToSchema, Arbitrary, Generic)

data WireIdP = WireIdP
  { _team :: TeamId,
    -- | list of issuer names that this idp has replaced, most recent first.  this is used
    -- for finding users that are still stored under the old issuer, see
    -- 'findUserWithOldIssuer', 'moveUserToNewIssuer'.
    _apiVersion :: Maybe WireIdPAPIVersion,
    _oldIssuers :: [SAML.Issuer],
    -- | the issuer that has replaced this one.  this is set iff a new issuer is created
    -- with the @"replaces"@ query parameter, and it is used to decide whether users not
    -- existing on this IdP can be auto-provisioned (if 'isJust', they can't).
    _replacedBy :: Maybe SAML.IdPId,
    _handle :: IdPHandle
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform WireIdP)

data WireIdPAPIVersion
  = -- | initial API
    WireIdPAPIV1
  | -- | support for different SP entityIDs per team
    WireIdPAPIV2
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform WireIdPAPIVersion)

-- | (Internal issue for making v2 the default:
-- https://wearezeta.atlassian.net/browse/SQSERVICES-781.  BEWARE: We probably shouldn't ever
-- do this, but remove V1 entirely instead.  which requires migrating away from the old table
-- on all on-prem installations.  which takes time.)
defWireIdPAPIVersion :: WireIdPAPIVersion
defWireIdPAPIVersion = WireIdPAPIV1

makeLenses ''WireIdP

deriveJSON deriveJSONOptions ''WireIdPAPIVersion

-- Changing the encoder since we've dropped the field prefixes
deriveJSON (defaultOptsDropChar '_') ''WireIdP

instance BSC.ToByteString WireIdPAPIVersion where
  builder =
    BSB.fromByteString . \case
      WireIdPAPIV1 -> "v1"
      WireIdPAPIV2 -> "v2"

instance BSC.FromByteString WireIdPAPIVersion where
  parser =
    (AP.string "v1" >> pure WireIdPAPIV1)
      <|> (AP.string "v2" >> pure WireIdPAPIV2)

instance FromHttpApiData WireIdPAPIVersion where
  parseQueryParam txt =
    maybe err Right $
      (BSC.fromByteString' . fromStrict . encodeUtf8) txt
    where
      err = Left $ "FromHttpApiData WireIdPAPIVersion: " <> txt

instance ToHttpApiData WireIdPAPIVersion where
  toQueryParam = decodeUtf8With lenientDecode . BSC.toByteString'

instance ToParamSchema WireIdPAPIVersion where
  toParamSchema Proxy =
    mempty
      { _schemaDefault = Just "v2",
        _schemaType = Just OpenApiString,
        _schemaEnum = Just (String . toQueryParam <$> [(minBound :: WireIdPAPIVersion) ..])
      }

instance Cql.Cql WireIdPAPIVersion where
  ctype = Cql.Tagged Cql.IntColumn

  toCql WireIdPAPIV1 = Cql.CqlInt 1
  toCql WireIdPAPIV2 = Cql.CqlInt 2

  fromCql (Cql.CqlInt i) = case i of
    1 -> pure WireIdPAPIV1
    2 -> pure WireIdPAPIV2
    n -> Left $ "Unexpected ClientCapability value: " ++ show n
  fromCql _ = Left "ClientCapability value: int expected"

-- | A list of 'IdP's, returned by some endpoints. Wrapped into an object to
-- allow extensibility later on.
newtype IdPList = IdPList {providers :: [IdP]}
  deriving (Eq, Show, Generic)

-- Same as WireIdP, we want the lenses, so we have to drop a prefix
deriveJSON (defaultOptsDropChar '_') ''IdPList

-- | JSON-encoded information about metadata: @{"value": <xml>}@.  (Here we could also
-- implement @{"uri": <url>, "cert": <pinned_pubkey>}@.  check both the certificate we get
-- from the server against the pinned one and the metadata url in the metadata against the one
-- we fetched the xml from, but it's unclear what the benefit would be.)
data IdPMetadataInfo = IdPMetadataValue Text SAML.IdPMetadata
  deriving (Eq, Show, Generic)

-- | We want to store the raw xml text from the registration request in the database for
-- trouble shooting, but @SAML.XML@ only gives us access to the xml tree, not the raw text.
-- 'RawXML' helps with that.
data RawXML

instance Accept RawXML where
  contentType Proxy = "application" // "xml"

instance MimeUnrender RawXML IdPMetadataInfo where
  mimeUnrender Proxy raw =
    IdPMetadataValue
      (decodeUtf8With lenientDecode . toStrict $ raw)
      <$> mimeUnrender (Proxy @SAML.XML) raw

instance MimeRender RawXML RawIdPMetadata where
  mimeRender Proxy (RawIdPMetadata raw) = fromStrict . encodeUtf8 $ raw

newtype RawIdPMetadata = RawIdPMetadata Text
  deriving (Eq, Show, Generic)

instance FromJSON IdPMetadataInfo where
  parseJSON = withObject "IdPMetadataInfo" $ \obj -> do
    raw <- obj .: "value"
    either fail (pure . IdPMetadataValue raw) (SAML.decode (LT.fromStrict raw))

instance ToJSON IdPMetadataInfo where
  toJSON (IdPMetadataValue _ x) =
    object ["value" .= SAML.encode x]

idPMetadataToInfo :: SAML.IdPMetadata -> IdPMetadataInfo
idPMetadataToInfo =
  -- 'undefined' is fine because `instance toJSON IdPMetadataValue` ignores it.  'fromJust' is
  -- ok as long as 'parseJSON . toJSON' always yields a value and not 'Nothing'.
  fromJust . parseMaybe parseJSON . toJSON . IdPMetadataValue undefined

-- Swagger instances

-- Same as WireIdP, check there for why this has different handling
instance ToSchema IdPList where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions $ defaultOptsDropChar '_'

instance ToSchema WireIdPAPIVersion where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema WireIdP where
  -- We don't want to use `samlSchemaOptions`, as it pulls from saml2-web-sso json options which
  -- as a `dropWhile not . isUpper` modifier. All we need is to drop the underscore prefix and
  -- keep the rest of the default processing. This isn't strictly in line with WPB-3798's requirements
  -- but it is close, and maintains the lens template haskell.
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions $ defaultOptsDropChar '_'

-- TODO: would be nice to add an example here, but that only works for json?

instance ToSchema RawIdPMetadata where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema IdPMetadataInfo where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "IdPMetadataInfo") $
        mempty
          & properties
            .~ properties_
          & minProperties
            ?~ 1
          & maxProperties
            ?~ 1
          & type_
            ?~ OpenApiObject
    where
      properties_ :: InsOrdHashMap Text (Referenced Schema)
      properties_ =
        InsOrdHashMap.fromList
          [ ("value", Inline (toSchema (Proxy @String)))
          ]
