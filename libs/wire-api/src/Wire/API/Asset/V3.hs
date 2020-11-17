{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Asset.V3
  ( -- * Asset
    Asset,
    mkAsset,
    assetKey,
    assetExpires,
    assetToken,

    -- * AssetKey
    AssetKey (..),

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
  )
where

import qualified Codec.MIME.Type as MIME
import Control.Lens (makeLenses)
import Crypto.Hash (Digest, MD5, hashlazy)
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteArray as B
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import Data.Id
import Data.Json.Util (toUTCTimeMillis, (#))
import Data.Text.Ascii (AsciiBase64Url)
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import qualified Data.UUID as UUID
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- Asset

-- | A newly uploaded asset.
data Asset = Asset
  { _assetKey :: AssetKey,
    _assetExpires :: Maybe UTCTime,
    _assetToken :: Maybe AssetToken
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Asset)

mkAsset :: AssetKey -> Asset
mkAsset k = Asset k Nothing Nothing

instance ToJSON Asset where
  toJSON a =
    object $
      "key" .= _assetKey a
        # "expires" .= fmap toUTCTimeMillis (_assetExpires a)
        # "token" .= _assetToken a
        # []

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o ->
    Asset
      <$> o .: "key"
      <*> o .:? "expires"
      <*> o .:? "token"

--------------------------------------------------------------------------------
-- AssetKey

-- | A unique, versioned asset identifier.
-- Note: Can be turned into a sum type with additional constructors
-- for future versions.
data AssetKey = AssetKeyV3 AssetId AssetRetention
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AssetKey)

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
          Just i -> return $! AssetKeyV3 (Id i) r
          Nothing -> fail "Invalid asset ID"

instance ToByteString AssetKey where
  builder (AssetKeyV3 i r) =
    builder '3'
      <> builder '-'
      <> builder r
      <> builder '-'
      <> builder (UUID.toASCIIBytes (toUUID i))

instance ToJSON AssetKey where
  toJSON = String . T.decodeUtf8 . toByteString'

instance FromJSON AssetKey where
  parseJSON =
    withText "AssetKey" $
      either fail pure . runParser parser . T.encodeUtf8

--------------------------------------------------------------------------------
-- AssetToken

-- | Asset tokens are bearer tokens that grant access to a single asset.
newtype AssetToken = AssetToken {assetTokenAscii :: AsciiBase64Url}
  deriving stock (Eq, Show)
  deriving newtype (FromByteString, ToByteString, FromJSON, ToJSON, Arbitrary)

-- | A newly (re)generated token for an existing asset.
newtype NewAssetToken = NewAssetToken
  {newAssetToken :: AssetToken}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

instance FromJSON NewAssetToken where
  parseJSON = withObject "NewAssetToken" $ \o ->
    NewAssetToken <$> o .: "token"

instance ToJSON NewAssetToken where
  toJSON (NewAssetToken tok) =
    object ["token" .= tok]

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
beginMultipartBody sets (AssetHeaders t l d) =
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
       \Content-MD5: "
    <> byteString (B64.encode (B.convert d))
    <> byteString
      "\r\n\
      \\r\n"
  where
    settingsJson = encode sets

-- | The trailer of a non-resumable @multipart/mixed@ request body initiated
-- via 'beginMultipartBody'.
endMultipartBody :: Builder
endMultipartBody = byteString "\r\n--frontier--\r\n"

--------------------------------------------------------------------------------
-- AssetHeaders

-- | Headers provided during upload.
data AssetHeaders = AssetHeaders
  { hdrType :: MIME.Type,
    hdrLength :: Word,
    hdrMD5 :: Digest MD5
  }

mkHeaders :: MIME.Type -> LByteString -> AssetHeaders
mkHeaders t b = AssetHeaders t (fromIntegral (LBS.length b)) (hashlazy b)

--------------------------------------------------------------------------------
-- AssetSettings

-- | Settings provided during upload.
data AssetSettings = AssetSettings
  { _setAssetPublic :: Bool,
    _setAssetRetention :: Maybe AssetRetention
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AssetSettings)

defAssetSettings :: AssetSettings
defAssetSettings = AssetSettings False Nothing

instance ToJSON AssetSettings where
  toJSON s =
    object $
      "public" .= _setAssetPublic s
        # "retention" .= _setAssetRetention s
        # []

instance FromJSON AssetSettings where
  parseJSON = withObject "AssetSettings" $ \o ->
    AssetSettings
      <$> o .:? "public" .!= False
      <*> o .:? "retention"

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
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform AssetRetention)

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
      1 -> return AssetEternal
      2 -> return AssetPersistent
      3 -> return AssetVolatile
      4 -> return AssetEternalInfrequentAccess
      5 -> return AssetExpiring
      _ -> fail $ "Invalid asset retention: " ++ show d

instance ToJSON AssetRetention where
  toJSON = String . retentionToTextRep

retentionToTextRep :: AssetRetention -> Text
retentionToTextRep AssetEternal = "eternal"
retentionToTextRep AssetPersistent = "persistent"
retentionToTextRep AssetVolatile = "volatile"
retentionToTextRep AssetEternalInfrequentAccess = "eternal-infrequent_access"
retentionToTextRep AssetExpiring = "expiring"

-- | JSON representation, used by AssetSettings are
instance FromJSON AssetRetention where
  parseJSON = withText "AssetRetention" $ \t ->
    case t of
      "eternal" -> pure AssetEternal
      "persistent" -> pure AssetPersistent
      "volatile" -> pure AssetVolatile
      "eternal-infrequent_access" -> pure AssetEternalInfrequentAccess
      "expiring" -> pure AssetExpiring
      _ -> fail $ "Invalid asset retention: " ++ show t

makeLenses ''Asset
makeLenses ''AssetSettings
