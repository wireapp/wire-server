{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

module Data.Nonce where

import Cassandra hiding (Value)
import qualified Data.Aeson as A
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Proxy (Proxy (Proxy))
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Swagger.ParamSchema
import Data.Text (pack)
import Data.Text.Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.UUID as UUID (UUID, fromASCIIBytes, fromByteString, toASCIIBytes, toByteString)
import Data.UUID.V4 (nextRandom)
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck (Arbitrary)

newtype Nonce = Nonce {unNonce :: UUID}
  deriving (Eq, Show)
  deriving newtype (A.FromJSON, A.ToJSON, S.ToSchema, Arbitrary)

instance ToByteString Nonce where
  builder = builder . Base64.encode . toStrict . UUID.toByteString . unNonce

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
  toQueryParam nonce = cs (toByteString' nonce)

instance FromHttpApiData Nonce where
  parseQueryParam s =
    first pack $ runParser parser (encodeUtf8 s)

randomNonce :: (Functor m, MonadIO m) => m Nonce
randomNonce = Nonce <$> liftIO nextRandom

isValidBase64UrlEncodedUUID :: ByteString -> Bool
isValidBase64UrlEncodedUUID bs = either (const False) (const True) $ runParser (parser @Nonce) bs

instance Cql Nonce where
  ctype = Tagged UuidColumn
  toCql = toCql . unNonce
  fromCql v = Nonce <$> fromCql v
