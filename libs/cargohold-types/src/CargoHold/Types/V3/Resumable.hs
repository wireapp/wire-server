{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module CargoHold.Types.V3.Resumable
  ( ResumableSettings,
    mkResumableSettings,
    setResumableType,
    setResumablePublic,
    setResumableRetention,
    ResumableAsset,
    TotalSize (..),
    ChunkSize (..),
    Offset (..),
    mkResumableAsset,
    resumableAsset,
    resumableExpires,
    resumableChunkSize,
  )
where

import CargoHold.Types.V3
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Conversion
import Data.Json.Util ((#), toUTCTimeMillis)
import Data.Time.Clock
import Imports

--------------------------------------------------------------------------------
-- ResumableSettings

-- | Settings for initiating a resumable upload.
data ResumableSettings
  = ResumableSettings
      { _setResumableRetention :: AssetRetention,
        _setResumablePublic :: Bool,
        _setResumableType :: MIME.Type
      }
  deriving (Show)

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
    ResumableSettings <$> o .:? "retention" .!= AssetPersistent
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

newtype TotalSize
  = TotalSize
      {totalSizeBytes :: Word}
  deriving
    ( Eq,
      Show,
      Ord,
      Num,
      Enum,
      Real,
      Integral,
      FromJSON,
      ToJSON,
      FromByteString,
      ToByteString
    )

newtype ChunkSize
  = ChunkSize
      {chunkSizeBytes :: Word}
  deriving
    ( Eq,
      Show,
      Ord,
      Num,
      Enum,
      Real,
      Integral,
      FromJSON,
      ToJSON,
      FromByteString,
      ToByteString
    )

newtype Offset
  = Offset
      {offsetBytes :: Word}
  deriving
    ( Eq,
      Show,
      Ord,
      Num,
      Enum,
      Real,
      Integral,
      FromJSON,
      ToJSON,
      FromByteString,
      ToByteString
    )

data ResumableAsset
  = ResumableAsset
      { _resumableAsset :: Asset,
        _resumableExpires :: UTCTime,
        _resumableChunkSize :: ChunkSize
      }

makeLenses ''ResumableAsset

mkResumableAsset :: Asset -> UTCTime -> ChunkSize -> ResumableAsset
mkResumableAsset = ResumableAsset

instance FromJSON ResumableAsset where
  parseJSON = withObject "ResumableAsset" $ \o ->
    ResumableAsset <$> o .: "asset"
      <*> o .: "expires"
      <*> o .: "chunk_size"

instance ToJSON ResumableAsset where
  toJSON r =
    object $
      "asset" .= _resumableAsset r
        # "expires" .= toUTCTimeMillis (_resumableExpires r)
        # "chunk_size" .= _resumableChunkSize r
        # []
