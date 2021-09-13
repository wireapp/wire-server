{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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

-- | Types for verification codes.
module Data.Code where

import Cassandra hiding (Value)
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Conversion
import Data.Json.Util
import Data.Proxy (Proxy (Proxy))
import Data.Range
import Data.Schema
import Data.Scientific (toBoundedInteger)
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Swagger.ParamSchema
import Data.Text (pack)
import Data.Text.Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck (Arbitrary (arbitrary))

-- | A scoped identifier for a 'Value' with an associated 'Timeout'.
newtype Key = Key {asciiKey :: Range 20 20 AsciiBase64Url}
  deriving (Eq, Show)
  deriving newtype
    ( A.FromJSON,
      A.ToJSON,
      ToSchema,
      S.ToSchema,
      FromByteString,
      ToByteString,
      Arbitrary
    )

instance ToParamSchema Key where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance FromHttpApiData Key where
  parseQueryParam s =
    first pack $ runParser parser (encodeUtf8 s)

instance ToHttpApiData Key where
  toQueryParam key = cs (toByteString' key)

-- | A secret value bound to a 'Key' and a 'Timeout'.
newtype Value = Value {asciiValue :: Range 6 20 AsciiBase64Url}
  deriving (Eq, Show)
  deriving newtype
    ( A.FromJSON,
      A.ToJSON,
      ToSchema,
      S.ToSchema,
      FromByteString,
      ToByteString,
      Arbitrary
    )

instance ToParamSchema Value where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance FromHttpApiData Value where
  parseQueryParam s =
    first pack $ runParser parser (encodeUtf8 s)

instance ToHttpApiData Value where
  toQueryParam key = cs (toByteString' key)

newtype Timeout = Timeout
  {timeoutDiffTime :: NominalDiffTime}
  deriving (Eq, Show, Ord, Enum, Num, Fractional, Real, RealFrac)
  deriving (S.ToSchema) via (Schema Timeout)

instance ToSchema Timeout where
  schema = Timeout . fromIntegral <$> (roundDiffTime . timeoutDiffTime) .= schema
    where
      roundDiffTime :: NominalDiffTime -> Int32
      roundDiffTime = round

-- | A 'Timeout' is rendered as an integer representing the number of seconds remaining.
instance ToByteString Timeout where
  builder (Timeout t) = builder (round t :: Int32)

-- | A 'Timeout' is rendered in JSON as an integer representing the
-- number of seconds remaining.
instance A.ToJSON Timeout where
  toJSON (Timeout t) = A.toJSON (round t :: Int32)

-- | A 'Timeout' is parsed from JSON as an integer representing the
-- number of seconds remaining.
instance A.FromJSON Timeout where
  parseJSON = A.withScientific "Timeout" $ \n ->
    let t = toBoundedInteger n :: Maybe Int32
     in maybe
          (fail "Invalid timeout value")
          (pure . Timeout . fromIntegral)
          t

instance Arbitrary Timeout where
  arbitrary = Timeout . fromIntegral <$> arbitrary @Int

deriving instance Cql Key

deriving instance Cql Value

-- | A key/value pair. This would actually more accurately if the value would actually
-- be a "value" but since we use "key" and "code" already in quite a few place in the API
-- (but without a type, using plain fields). This will make it easier to re-use a key/value
-- pair in the API, keeping "code" in the JSON for backwards compatibility
data KeyValuePair = KeyValuePair
  { kcKey :: !Key,
    kcCode :: !Value
  }
  deriving (Eq, Generic, Show)

deriveJSON toJSONFieldName ''KeyValuePair
