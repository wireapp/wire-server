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

module Wire.API.MLS.CipherSuite
  ( -- * MLS ciphersuites
    CipherSuite (..),
    defCipherSuite,
    CipherSuiteTag (..),
    cipherSuiteTag,
    tagCipherSuite,

    -- * MLS signature schemes
    SignatureScheme (..),
    SignatureSchemeTag (..),
    signatureScheme,
    signatureSchemeName,
    signatureSchemeTag,
    csSignatureScheme,

    -- * Utilities
    csHash,
    csVerifySignatureWithLabel,
    csVerifySignature,
    signWithLabel,
  )
where

import Cassandra qualified as C
import Cassandra.CQL
import Control.Applicative
import Control.Error (note)
import Control.Lens ((?~))
import Crypto.Error
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Bifunctor
import Data.ByteArray hiding (index)
import Data.ByteArray qualified as BA
import Data.ByteString.Conversion
import Data.OpenApi qualified as S
import Data.OpenApi.Internal.Schema qualified as S
import Data.Proxy
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Text.Lazy.Builder.Int qualified as LT
import Data.Word
import Imports
import Web.HttpApiData
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

newtype CipherSuite = CipherSuite {cipherSuiteNumber :: Word16}
  deriving stock (Eq, Show)
  deriving newtype (ParseMLS, SerialiseMLS, Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CipherSuite

instance ToSchema CipherSuite where
  schema =
    named "CipherSuite" $
      cipherSuiteNumber .= fmap CipherSuite (unnamed schema)

instance S.ToParamSchema CipherSuite where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiNumber

instance FromHttpApiData CipherSuite where
  parseUrlPiece = parseHeader . T.encodeUtf8
  parseHeader = first T.pack . runParser parser

instance ToHttpApiData CipherSuite where
  toUrlPiece =
    LT.toStrict
      . LT.toLazyText
      . ("0x" <>)
      . LT.hexadecimal
      . cipherSuiteNumber

instance FromByteString CipherSuite where
  parser = do
    void $ Atto.try (optional (Atto.string "0x"))
    CipherSuite <$> Atto.hexadecimal

data CipherSuiteTag
  = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  | MLS_128_X25519Kyber768Draft00_AES128GCM_SHA256_Ed25519
  deriving stock (Bounded, Enum, Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform CipherSuiteTag)

defCipherSuite :: CipherSuiteTag
defCipherSuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519

instance S.ToSchema CipherSuiteTag where
  declareNamedSchema _ =
    pure . S.named "CipherSuiteTag" $
      ( S.paramSchemaToSchema (Proxy @Word16)
          & S.description ?~ "Index number of ciphersuite. See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5"
      )

instance ToSchema CipherSuiteTag where
  schema =
    mkSchema
      (swaggerDoc @CipherSuiteTag)
      tagParser
      (Just . toJSON . cipherSuiteNumber . tagCipherSuite)
    where
      tagParser v = do
        index <- parseJSON v
        maybe
          (fail "Not a valid index number of a ciphersuite. See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5.")
          pure
          (cipherSuiteTag (CipherSuite index))

instance C.Cql CipherSuiteTag where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . cipherSuiteNumber . tagCipherSuite

  fromCql (CqlInt index) =
    case cipherSuiteTag (CipherSuite (fromIntegral index)) of
      Just t -> Right t
      Nothing -> Left "CipherSuiteTag: unexpected index"
  fromCql _ = Left "CipherSuiteTag: int expected"

-- | See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5.
cipherSuiteTag :: CipherSuite -> Maybe CipherSuiteTag
cipherSuiteTag cs = listToMaybe $ do
  t <- [minBound .. maxBound]
  guard (tagCipherSuite t == cs)
  pure t

-- | Inverse of 'cipherSuiteTag'
tagCipherSuite :: CipherSuiteTag -> CipherSuite
tagCipherSuite MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = CipherSuite 1
tagCipherSuite MLS_128_X25519Kyber768Draft00_AES128GCM_SHA256_Ed25519 = CipherSuite 0xf031

csHash :: CipherSuiteTag -> ByteString -> RawMLS a -> ByteString
csHash MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = sha256Hash
csHash MLS_128_X25519Kyber768Draft00_AES128GCM_SHA256_Ed25519 = sha256Hash

sha256Hash :: ByteString -> RawMLS a -> ByteString
sha256Hash ctx value = convert . hashWith SHA256 . encodeMLS' $ RefHashInput ctx value

csVerifySignature :: CipherSuiteTag -> ByteString -> RawMLS a -> ByteString -> Bool
csVerifySignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = ed25519VerifySignature
csVerifySignature MLS_128_X25519Kyber768Draft00_AES128GCM_SHA256_Ed25519 = ed25519VerifySignature

ed25519VerifySignature :: ByteString -> RawMLS a -> ByteString -> Bool
ed25519VerifySignature pub x sig =
  fromMaybe False . maybeCryptoError $ do
    pub' <- Ed25519.publicKey pub
    sig' <- Ed25519.signature sig
    pure $ Ed25519.verify pub' x.raw sig'

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-5.2-5
type RefHashInput = SignContent

pattern RefHashInput :: ByteString -> RawMLS a -> RefHashInput a
pattern RefHashInput label content = SignContent label content

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-5.1.2-6
data SignContent a = SignContent
  { sigLabel :: ByteString,
    content :: RawMLS a
  }

instance SerialiseMLS (SignContent a) where
  serialiseMLS c = do
    serialiseMLSBytes @VarInt c.sigLabel
    serialiseMLSBytes @VarInt c.content.raw

mkSignContent :: ByteString -> RawMLS a -> SignContent a
mkSignContent sigLabel content =
  SignContent
    { sigLabel = "MLS 1.0 " <> sigLabel,
      content = content
    }

csVerifySignatureWithLabel ::
  CipherSuiteTag ->
  ByteString ->
  ByteString ->
  RawMLS a ->
  ByteString ->
  Bool
csVerifySignatureWithLabel cs pub label x sig =
  csVerifySignature cs pub (mkRawMLS (mkSignContent label x)) sig

-- FUTUREWORK: generalise to arbitrary ciphersuites
signWithLabel :: ByteString -> Ed25519.SecretKey -> Ed25519.PublicKey -> RawMLS a -> ByteString
signWithLabel sigLabel priv pub x = BA.convert $ Ed25519.sign priv pub (encodeMLS' (mkSignContent sigLabel x))

csSignatureScheme :: CipherSuiteTag -> SignatureSchemeTag
csSignatureScheme MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = Ed25519
csSignatureScheme MLS_128_X25519Kyber768Draft00_AES128GCM_SHA256_Ed25519 = Ed25519

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

instance Cql SignatureSchemeTag where
  ctype = Tagged TextColumn
  toCql = CqlText . signatureSchemeName
  fromCql (CqlText name) =
    note ("Unexpected signature scheme: " <> T.unpack name) $
      signatureSchemeFromName name
  fromCql _ = Left "SignatureScheme: Text expected"

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

instance S.ToParamSchema SignatureSchemeTag where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

instance FromHttpApiData SignatureSchemeTag where
  parseQueryParam = note "Unknown signature scheme" . signatureSchemeFromName

instance ToJSON SignatureSchemeTag where
  toJSON = Aeson.String . signatureSchemeName

instance ToJSONKey SignatureSchemeTag where
  toJSONKey = Aeson.toJSONKeyText signatureSchemeName

instance S.ToSchema SignatureSchemeTag where
  declareNamedSchema _ = S.declareNamedSchema (Proxy @Text)
