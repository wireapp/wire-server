{-# LANGUAGE DataKinds #-}
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

module Data.Nonce
  ( Nonce (..),
    NonceTtlSecs (..),
    randomNonce,
    isValidBase64UrlEncodedUUID,
  )
where

import Cassandra hiding (Value)
import Data.Aeson qualified as A
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.OpenApi qualified as S
import Data.OpenApi.ParamSchema
import Data.Proxy (Proxy (Proxy))
import Data.Schema
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.UUID as UUID (UUID, fromByteString, toByteString)
import Data.UUID.V4 (nextRandom)
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.UUID ()

newtype Nonce = Nonce {unNonce :: UUID}
  deriving (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema Nonce)

instance ToSchema Nonce where
  schema =
    (decodeUtf8With lenientDecode . toByteString') .= parsedText "Nonce" p
    where
      p :: Text -> Either String Nonce
      p =
        maybe (Left "Invalid Nonce") Right
          . fromByteString'
          . fromStrict
          . encodeUtf8

instance ToByteString Nonce where
  builder = builder . Base64.encodeUnpadded . toStrict . UUID.toByteString . unNonce

instance FromByteString Nonce where
  parser = do
    a <- parser
    maybe
      (fail "invalid base64url encoded uuidv4")
      (pure . Nonce)
      (either (const Nothing) (UUID.fromByteString . fromStrict) (Base64.decode a))

instance ToParamSchema Nonce where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToHttpApiData Nonce where
  toQueryParam = decodeUtf8With lenientDecode . toByteString'

instance FromHttpApiData Nonce where
  parseQueryParam =
    maybe (Left "Invalid Nonce") Right
      . fromByteString'
      . fromStrict
      . encodeUtf8

randomNonce :: (MonadIO m) => m Nonce
randomNonce = Nonce <$> liftIO nextRandom

isValidBase64UrlEncodedUUID :: ByteString -> Bool
isValidBase64UrlEncodedUUID = isJust . fromByteString' @Nonce . fromStrict

instance Cql Nonce where
  ctype = Tagged UuidColumn
  toCql = toCql . unNonce
  fromCql v = Nonce <$> fromCql v

newtype NonceTtlSecs = NonceTtlSecs {unNonceTtlSecs :: Word32}
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema NonceTtlSecs)

-- | Convert 'Word32' to 'Int32', with clipping if it doesn't fit.
word32ToInt32 :: Word32 -> Int32
word32ToInt32 = fromIntegral . min (fromIntegral (maxBound @Int32))

-- | Convert 'Int32' to 'Word32', rounding negative values to 0.
int32ToWord32 :: Int32 -> Word32
int32ToWord32 = fromIntegral . max 0

instance ToSchema NonceTtlSecs where
  schema = NonceTtlSecs . int32ToWord32 <$> (word32ToInt32 . unNonceTtlSecs) .= schema

instance Cql NonceTtlSecs where
  ctype = Tagged IntColumn
  toCql = CqlInt . (word32ToInt32 . unNonceTtlSecs)
  fromCql (CqlInt i) = pure $ NonceTtlSecs $ int32ToWord32 i
  fromCql _ = Left "fromCql: NonceTtlSecs expects CqlInt"
