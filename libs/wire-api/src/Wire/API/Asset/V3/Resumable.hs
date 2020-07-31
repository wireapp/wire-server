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

module Wire.API.Asset.V3.Resumable
  ( -- * ResumableSettings
    ResumableSettings,
    mkResumableSettings,
    setResumableType,
    setResumablePublic,
    setResumableRetention,

    -- * ResumableAsset
    ResumableAsset,
    mkResumableAsset,
    TotalSize (..),
    ChunkSize (..),
    Offset (..),
    resumableAsset,
    resumableExpires,
    resumableChunkSize,
  )
where

import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Conversion
import Data.Json.Util (toUTCTimeMillis, (#))
import Data.Time.Clock
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Asset.V3

--------------------------------------------------------------------------------
-- ResumableSettings

-- | Settings for initiating a resumable upload.
data ResumableSettings = ResumableSettings
  { _setResumableRetention :: AssetRetention,
    _setResumablePublic :: Bool,
    _setResumableType :: MIME.Type
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ResumableSettings)

makeLenses ''ResumableSettings

mkResumableSettings :: AssetRetention -> Bool -> MIME.Type -> ResumableSettings
mkResumableSettings = ResumableSettings

instance ToJSON ResumableSettings where
  toJSON (ResumableSettings ret pub typ) =
    object $
      "retention" .= ret
        # "type" .= MIME.showType typ
        # "public" .= pub
        # []

instance FromJSON ResumableSettings where
  parseJSON = withObject "ResumableSettings" $ \o ->
    ResumableSettings
      <$> o .:? "retention" .!= AssetPersistent
      <*> o .:? "public" .!= False
      <*> (parseMime =<< o .: "type")

parseMime :: Text -> Parser MIME.Type
parseMime v =
  maybe
    (fail "Invalid MIME type")
    return
    (MIME.parseMIMEType v)

--------------------------------------------------------------------------------
-- ResumableAsset

newtype TotalSize = TotalSize
  {totalSizeBytes :: Word}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral, FromJSON, ToJSON, FromByteString, ToByteString, Arbitrary)

newtype ChunkSize = ChunkSize
  {chunkSizeBytes :: Word}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral, FromJSON, ToJSON, FromByteString, ToByteString, Arbitrary)

newtype Offset = Offset
  {offsetBytes :: Word}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral, FromJSON, ToJSON, FromByteString, ToByteString, Arbitrary)

data ResumableAsset = ResumableAsset
  { _resumableAsset :: Asset,
    _resumableExpires :: UTCTime,
    _resumableChunkSize :: ChunkSize
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ResumableAsset)

makeLenses ''ResumableAsset

mkResumableAsset :: Asset -> UTCTime -> ChunkSize -> ResumableAsset
mkResumableAsset = ResumableAsset

instance ToJSON ResumableAsset where
  toJSON r =
    object $
      "asset" .= _resumableAsset r
        # "expires" .= toUTCTimeMillis (_resumableExpires r)
        # "chunk_size" .= _resumableChunkSize r
        # []

instance FromJSON ResumableAsset where
  parseJSON = withObject "ResumableAsset" $ \o ->
    ResumableAsset
      <$> o .: "asset"
      <*> o .: "expires"
      <*> o .: "chunk_size"
