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

module Wire.API.User.IdentityProvider where

import qualified Cassandra as Cql
import Control.Exception (assert)
import Control.Lens (makeLenses, (.~), (?~))
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id (TeamId)
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions
import Data.Swagger
import Imports
import Network.HTTP.Media ((//))
import SAML2.WebSSO (IdPConfig)
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Servant.API as Servant hiding (MkLink, URI (..))
import Wire.API.User.Orphans (samlSchemaOptions)

-- | The identity provider type used in Spar.
type IdP = IdPConfig WireIdP

data WireIdP = WireIdP
  { _wiTeam :: TeamId,
    -- | list of issuer names that this idp has replaced, most recent first.  this is used
    -- for finding users that are still stored under the old issuer, see
    -- 'findUserWithOldIssuer', 'moveUserToNewIssuer'.
    _wiApiVersion :: Maybe WireIdPAPIVersion,
    _wiOldIssuers :: [SAML.Issuer],
    -- | the issuer that has replaced this one.  this is set iff a new issuer is created
    -- with the @"replaces"@ query parameter, and it is used to decide whether users not
    -- existing on this IdP can be auto-provisioned (if 'isJust', they can't).
    _wiReplacedBy :: Maybe SAML.IdPId
  }
  deriving (Eq, Show, Generic)

data WireIdPAPIVersion
  = -- | initial API
    WireIdPAPIV1
  | -- | support for different SP entityIDs per team
    WireIdPAPIV2
  deriving (Eq, Show, Enum, Bounded, Generic)

defWireIdPAPIVersion :: WireIdPAPIVersion
defWireIdPAPIVersion = WireIdPAPIV2

makeLenses ''WireIdP

deriveJSON deriveJSONOptions ''WireIdPAPIVersion
deriveJSON deriveJSONOptions ''WireIdP

instance Cql.Cql WireIdPAPIVersion where
  ctype = Cql.Tagged Cql.IntColumn

  toCql WireIdPAPIV1 = Cql.CqlInt 1
  toCql WireIdPAPIV2 = Cql.CqlInt 2

  fromCql (Cql.CqlInt i) = case i of
    1 -> return WireIdPAPIV1
    2 -> return WireIdPAPIV2
    n -> Left $ "Unexpected ClientCapability value: " ++ show n
  fromCql _ = Left "ClientCapability value: int expected"

-- | A list of 'IdP's, returned by some endpoints. Wrapped into an object to
-- allow extensibility later on.
data IdPList = IdPList
  { _idplProviders :: [IdP]
  }
  deriving (Eq, Show, Generic)

makeLenses ''IdPList

deriveJSON deriveJSONOptions ''IdPList

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
  mimeUnrender Proxy raw = IdPMetadataValue (cs raw) <$> mimeUnrender (Proxy @SAML.XML) raw

instance MimeRender RawXML RawIdPMetadata where
  mimeRender Proxy (RawIdPMetadata raw) = cs raw

newtype RawIdPMetadata = RawIdPMetadata Text
  deriving (Eq, Show, Generic)

instance FromJSON IdPMetadataInfo where
  parseJSON = withObject "IdPMetadataInfo" $ \obj -> do
    raw <- obj .: "value"
    either fail (pure . IdPMetadataValue raw) (SAML.decode (cs raw))

instance ToJSON IdPMetadataInfo where
  toJSON (IdPMetadataValue _ x) =
    object ["value" .= SAML.encode x]

-- Swagger instances

instance ToSchema IdPList where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema WireIdPAPIVersion where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema WireIdP where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

-- TODO: would be nice to add an example here, but that only works for json?

instance ToSchema RawIdPMetadata where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

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
        InsOrdHashMap.fromList
          [ ("value", Inline (toSchema (Proxy @String)))
          ]
