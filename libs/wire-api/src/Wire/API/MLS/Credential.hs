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

module Wire.API.MLS.Credential where

import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Binary
import Data.Binary.Get
import Data.Binary.Parser
import Data.Binary.Parser.Char8
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as T
import Data.UUID
import Imports
import Wire.API.Arbitrary
import Wire.API.MLS.Serialisation

-- | An MLS credential.
--
-- Only the @BasicCredential@ type is supported.
data Credential = BasicCredential
  { bcIdentity :: ByteString,
    bcSignatureScheme :: SignatureScheme,
    bcSignatureKey :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Credential

data CredentialTag = BasicCredentialTag
  deriving stock (Enum, Bounded, Eq, Show)

instance ParseMLS CredentialTag where
  parseMLS = parseMLSEnum @Word16 "credential type"

instance ParseMLS Credential where
  parseMLS =
    parseMLS >>= \case
      BasicCredentialTag ->
        BasicCredential
          <$> parseMLSBytes @Word16
          <*> parseMLS
          <*> parseMLSBytes @Word16

credentialTag :: Credential -> CredentialTag
credentialTag (BasicCredential _ _ _) = BasicCredentialTag

-- | A TLS signature scheme.
--
-- See <https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#tls-signaturescheme>.
newtype SignatureScheme = SignatureScheme {unSignatureScheme :: Word16}
  deriving stock (Eq, Show)
  deriving newtype (ParseMLS, Arbitrary)

signatureScheme :: SignatureSchemeTag -> SignatureScheme
signatureScheme = SignatureScheme . signatureSchemeNumber

data SignatureSchemeTag = Ed25519
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform SignatureSchemeTag

signatureSchemeNumber :: SignatureSchemeTag -> Word16
signatureSchemeNumber Ed25519 = 0x807

signatureSchemeName :: SignatureSchemeTag -> Text
signatureSchemeName Ed25519 = "ed25519"

signatureSchemeTag :: SignatureScheme -> Maybe SignatureSchemeTag
signatureSchemeTag (SignatureScheme n) = getAlt $
  flip foldMap [minBound .. maxBound] $ \s ->
    guard (signatureSchemeNumber s == n) $> s

signatureSchemeFromName :: Text -> Maybe SignatureSchemeTag
signatureSchemeFromName name = getAlt $
  flip foldMap [minBound .. maxBound] $ \s ->
    guard (signatureSchemeName s == name) $> s

parseSignatureScheme :: MonadFail f => Text -> f SignatureSchemeTag
parseSignatureScheme name =
  maybe
    (fail ("Unsupported signature scheme " <> T.unpack name))
    pure
    (signatureSchemeFromName name)

instance FromJSON SignatureSchemeTag where
  parseJSON = Aeson.withText "SignatureScheme" parseSignatureScheme

instance FromJSONKey SignatureSchemeTag where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseSignatureScheme

instance ToJSON SignatureSchemeTag where
  toJSON = Aeson.String . signatureSchemeName

instance ToJSONKey SignatureSchemeTag where
  toJSONKey = Aeson.toJSONKeyText signatureSchemeName

data ClientIdentity = ClientIdentity
  { ciDomain :: Domain,
    ciUser :: UserId,
    ciClient :: ClientId
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientIdentity

cidQualifiedClient :: ClientIdentity -> Qualified (UserId, ClientId)
cidQualifiedClient cid = Qualified (ciUser cid, ciClient cid) (ciDomain cid)

instance ToSchema ClientIdentity where
  schema =
    object "ClientIdentity" $
      ClientIdentity
        <$> ciDomain .= field "domain" schema
        <*> ciUser .= field "user_id" schema
        <*> ciClient .= field "client_id" schema

instance ParseMLS ClientIdentity where
  parseMLS = do
    uid <-
      maybe (fail "Invalid UUID") (pure . Id)
        =<< fmap fromASCIIBytes (getByteString 36)
    char ':'
    cid <- newClientId <$> hexadecimal
    char '@'
    dom <-
      either fail pure
        =<< fmap (mkDomain . T.pack) (many' anyChar)
    pure $ ClientIdentity dom uid cid

mkClientIdentity :: Qualified UserId -> ClientId -> ClientIdentity
mkClientIdentity (Qualified uid domain) cid = ClientIdentity domain uid cid
