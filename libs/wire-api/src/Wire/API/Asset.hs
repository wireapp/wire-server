{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Asset
  ( -- * Asset
    Asset,
    Asset',
    mkAsset,
    assetKey,
    assetExpires,
    assetToken,

    -- * AssetKey
    AssetKey (..),
    assetKeyToText,

    -- * AssetToken
    AssetToken (..),
    NewAssetToken (..),

    -- * Body Construction
    buildMultipartBody,
    beginMultipartBody,
    endMultipartBody,

    -- * AssetHeaders
    AssetHeaders (..),
    mkHeaders,

    -- * AssetSettings
    AssetSettings,
    defAssetSettings,
    setAssetPublic,
    setAssetRetention,
    AssetRetention (..),
    assetRetentionSeconds,
    assetExpiringSeconds,
    assetVolatileSeconds,
    retentionToTextRep,

    -- * Streaming
    AssetLocation (..),
    LocalOrRemoteAsset (..),
  )
where

import Cassandra qualified as C
import Codec.MIME.Type qualified as MIME
import Control.Lens (makeLenses, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString.Char8 hiding (I)
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis), toUTCTimeMillis)
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Qualified
import Data.SOP
import Data.Schema
import Data.Text qualified as T
import Data.Text.Ascii (AsciiBase64Url)
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Time.Clock
import Data.UUID qualified as UUID
import Imports
import Servant
import URI.ByteString
import Wire.API.Error
import Wire.API.Routes.MultiVerb
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

--------------------------------------------------------------------------------
-- Asset

type Asset = Asset' (Qualified AssetKey)

-- | A newly uploaded asset.
data Asset' key = Asset
  { _assetKey :: key,
    _assetExpires :: Maybe UTCTime,
    _assetToken :: Maybe AssetToken
  }
  deriving stock (Eq, Show, Generic, Functor)

deriving via Schema (Asset' key) instance (ToSchema (Asset' key)) => (ToJSON (Asset' key))

deriving via Schema (Asset' key) instance (ToSchema (Asset' key)) => (FromJSON (Asset' key))

deriving via Schema (Asset' key) instance (Typeable key, ToSchema (Asset' key)) => (S.ToSchema (Asset' key))

-- Generate expiry time with millisecond precision
instance (Arbitrary key) => Arbitrary (Asset' key) where
  arbitrary = Asset <$> arbitrary <*> (fmap milli <$> arbitrary) <*> arbitrary
    where
      milli = fromUTCTimeMillis . toUTCTimeMillis

mkAsset :: key -> Asset' key
mkAsset k = Asset k Nothing Nothing

instance ToSchema Asset where
  schema =
    object "Asset" $
      Asset
        <$> _assetKey
          .= ( Qualified
                 <$> qUnqualified .= field "key" schema
                 <*> qDomain .= field "domain" schema
             )
        <*> (fmap toUTCTimeMillis . _assetExpires)
          .= maybe_
            (optField "expires" (fromUTCTimeMillis <$> schema))
        <*> _assetToken .= maybe_ (optField "token" schema)

--------------------------------------------------------------------------------
-- AssetKey

-- | A unique, versioned asset identifier.
-- Note: Can be turned into a sum type with additional constructors
-- for future versions.
data AssetKey = AssetKeyV3 AssetId AssetRetention
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AssetKey)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AssetKey)

instance FromByteString AssetKey where
  parser = do
    v <- decimal
    _ <- char '-'
    case (v :: Word) of
      3 -> parseV3
      _ -> fail $ "Invalid asset version: " ++ show v
    where
      -- AssetKeyV3 ::= Retention "-" uuid
      -- Retention  ::= decimal
      parseV3 = do
        r <- parser
        _ <- char '-'
        b <- takeByteString
        case UUID.fromASCIIBytes b of
          Just i -> pure $! AssetKeyV3 (Id i) r
          Nothing -> fail "Invalid asset ID"

instance ToByteString AssetKey where
  builder (AssetKeyV3 i r) =
    builder '3'
      <> builder '-'
      <> builder r
      <> builder '-'
      <> builder (UUID.toASCIIBytes (toUUID i))

assetKeyToText :: AssetKey -> Text
assetKeyToText = T.decodeUtf8 . toByteString'

instance ToSchema AssetKey where
  schema =
    assetKeyToText
      .= parsedTextWithDoc desc "AssetKey" (runParser parser . T.encodeUtf8)
      & doc' . S.schema . S.example ?~ toJSON ("3-1-47de4580-ae51-4650-acbb-d10c028cb0ac" :: Text)
    where
      desc =
        "S3 asset key for an icon image with retention information."

instance S.ToParamSchema AssetKey where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData AssetKey where
  parseUrlPiece = first T.pack . runParser parser . T.encodeUtf8

instance C.Cql AssetKey where
  ctype = C.Tagged C.TextColumn
  toCql = C.CqlText . assetKeyToText
  fromCql (C.CqlText txt) = runParser parser . T.encodeUtf8 $ txt
  fromCql _ = Left "AssetKey: Text expected"

--------------------------------------------------------------------------------
-- AssetToken

-- | Asset tokens are bearer tokens that grant access to a single asset.
newtype AssetToken = AssetToken {assetTokenAscii :: AsciiBase64Url}
  deriving stock (Eq, Show)
  deriving newtype (FromByteString, ToByteString, Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AssetToken)

instance ToSchema AssetToken where
  schema =
    AssetToken
      <$> assetTokenAscii
        .= schema
      & doc' . S.schema . S.example ?~ toJSON ("aGVsbG8" :: Text)

instance S.ToParamSchema AssetToken where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData AssetToken where
  parseUrlPiece = first T.pack . runParser parser . T.encodeUtf8

-- | A newly (re)generated token for an existing asset.
newtype NewAssetToken = NewAssetToken
  {newAssetToken :: AssetToken}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NewAssetToken)

instance ToSchema NewAssetToken where
  schema =
    object "NewAssetToken" $
      NewAssetToken <$> newAssetToken .= field "token" schema

--------------------------------------------------------------------------------
-- Body Construction

-- | Build a complete @multipart/mixed@ request body for a one-shot,
-- non-resumable asset upload.
buildMultipartBody :: AssetSettings -> MIME.Type -> LByteString -> Builder
buildMultipartBody sets typ bs =
  let hdrs = mkHeaders typ bs
   in beginMultipartBody sets hdrs <> lazyByteString bs <> endMultipartBody

-- | Begin building a @multipart/mixed@ request body for a non-resumable upload.
-- The returned 'Builder' can be immediately followed by the actual asset bytes.
beginMultipartBody :: AssetSettings -> AssetHeaders -> Builder
beginMultipartBody sets (AssetHeaders t l) =
  byteString
    "--frontier\r\n\
    \Content-Type: application/json\r\n\
    \Content-Length: "
    <> int64Dec (LBS.length settingsJson)
    <> byteString
      "\r\n\
      \\r\n"
    <> lazyByteString settingsJson
    <> byteString
      "\r\n\
      \--frontier\r\n\
      \Content-Type: "
    <> byteString (T.encodeUtf8 (MIME.showType t))
    <> byteString
      "\r\n\
      \Content-Length: "
    <> wordDec l
    <> "\r\n\
       \\r\n"
  where
    settingsJson = Aeson.encode (schemaToJSON sets)

-- | The trailer of a non-resumable @multipart/mixed@ request body initiated
-- via 'beginMultipartBody'.
endMultipartBody :: Builder
endMultipartBody = byteString "\r\n--frontier--\r\n"

--------------------------------------------------------------------------------
-- AssetHeaders

-- | Headers provided during upload.
data AssetHeaders = AssetHeaders
  { hdrType :: MIME.Type,
    hdrLength :: Word
  }

mkHeaders :: MIME.Type -> LByteString -> AssetHeaders
mkHeaders t b = AssetHeaders t (fromIntegral (LBS.length b))

--------------------------------------------------------------------------------
-- AssetSettings

-- | Settings provided during upload.
data AssetSettings = AssetSettings
  { _setAssetPublic :: Bool,
    _setAssetRetention :: Maybe AssetRetention
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AssetSettings)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AssetSettings)

defAssetSettings :: AssetSettings
defAssetSettings = AssetSettings False Nothing

instance ToSchema AssetSettings where
  schema =
    object "AssetSettings" $
      AssetSettings
        <$> _setAssetPublic .= (fromMaybe False <$> optField "public" schema)
        <*> _setAssetRetention .= maybe_ (optField "retention" schema)

--------------------------------------------------------------------------------
-- AssetRetention

-- | The desired asset retention.
data AssetRetention
  = -- | The asset is retained indefinitely. Typically used
    -- for profile pictures / assets frequently accessed.
    AssetEternal
  | -- | DEPRECATED: should not be used by clients for new assets
    -- The asset is retained indefinitely.
    AssetPersistent
  | -- | The asset is retained for a short period of time.
    AssetVolatile
  | -- | The asset is retained indefinitely, storage is optimised
    -- for infrequent access
    AssetEternalInfrequentAccess
  | -- | The asset is retained for an extended period of time,
    -- but not indefinitely.
    AssetExpiring
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform AssetRetention)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AssetRetention)

-- | The minimum TTL in seconds corresponding to a chosen retention.
assetRetentionSeconds :: AssetRetention -> Maybe NominalDiffTime
assetRetentionSeconds AssetEternal = Nothing
assetRetentionSeconds AssetPersistent = Nothing
assetRetentionSeconds AssetVolatile = Just assetVolatileSeconds
assetRetentionSeconds AssetEternalInfrequentAccess = Nothing
assetRetentionSeconds AssetExpiring = Just assetExpiringSeconds

assetVolatileSeconds :: NominalDiffTime
assetVolatileSeconds = 28 * 24 * 3600 -- 28 days

assetExpiringSeconds :: NominalDiffTime
assetExpiringSeconds = 365 * 24 * 3600 -- 365 days

instance ToByteString AssetRetention where
  builder AssetEternal = builder '1'
  builder AssetPersistent = builder '2'
  builder AssetVolatile = builder '3'
  builder AssetEternalInfrequentAccess = builder '4'
  builder AssetExpiring = builder '5'

-- | ByteString representation is used in AssetKey
instance FromByteString AssetRetention where
  parser =
    decimal >>= \d -> case (d :: Word) of
      1 -> pure AssetEternal
      2 -> pure AssetPersistent
      3 -> pure AssetVolatile
      4 -> pure AssetEternalInfrequentAccess
      5 -> pure AssetExpiring
      _ -> fail $ "Invalid asset retention: " ++ show d

retentionToTextRep :: AssetRetention -> Text
retentionToTextRep AssetEternal = "eternal"
retentionToTextRep AssetPersistent = "persistent"
retentionToTextRep AssetVolatile = "volatile"
retentionToTextRep AssetEternalInfrequentAccess = "eternal-infrequent_access"
retentionToTextRep AssetExpiring = "expiring"

instance ToSchema AssetRetention where
  schema =
    enum @Text "AssetRetention" $
      foldMap
        (\value -> element (retentionToTextRep value) value)
        [minBound .. maxBound]

-- FUTUREWORK: switch to a better URI library (e.g. modern-uri)
--
-- This URI type is error-prone, since its internal representation is based on
-- ByteString, whereas URLs are defined in terms of characters, not octets (RFC
-- 3986).
newtype AssetLocation r = AssetLocation {getAssetLocation :: URIRef r}

instance ToHttpApiData (AssetLocation r) where
  toUrlPiece = T.decodeUtf8With T.lenientDecode . toHeader
  toHeader = serializeURIRef' . getAssetLocation

instance FromHttpApiData (AssetLocation Relative) where
  parseUrlPiece = parseHeader . T.encodeUtf8
  parseHeader =
    bimap (T.pack . show) AssetLocation
      . parseRelativeRef strictURIParserOptions

instance FromHttpApiData (AssetLocation Absolute) where
  parseUrlPiece = parseHeader . T.encodeUtf8
  parseHeader =
    bimap (T.pack . show) AssetLocation
      . parseURI strictURIParserOptions

instance S.ToParamSchema (AssetLocation r) where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.format ?~ "url"

-- | An asset as returned by the download API: if the asset is local, only a
-- URL is returned, and if it is remote the content of the asset is streamed.
data LocalOrRemoteAsset
  = LocalAsset (AssetLocation Absolute)
  | RemoteAsset (SourceIO ByteString)

instance
  ( ResponseType r1 ~ AssetLocation Absolute,
    ResponseType r2 ~ SourceIO ByteString,
    KnownError (MapError e)
  ) =>
  AsUnion '[ErrorResponse e, r1, r2] (Maybe LocalOrRemoteAsset)
  where
  toUnion Nothing = Z (I (dynError @(MapError e)))
  toUnion (Just (LocalAsset loc)) = S (Z (I loc))
  toUnion (Just (RemoteAsset asset)) = S (S (Z (I asset)))

  fromUnion (Z (I _)) = Nothing
  fromUnion (S (Z (I loc))) = Just (LocalAsset loc)
  fromUnion (S (S (Z (I asset)))) = Just (RemoteAsset asset)
  fromUnion (S (S (S x))) = case x of {}

makeLenses ''Asset'
makeLenses ''AssetSettings
