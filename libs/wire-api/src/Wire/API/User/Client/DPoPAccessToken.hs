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

module Wire.API.User.Client.DPoPAccessToken where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (fromStrict)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), fromByteString', toByteString')
import Data.OpenApi qualified as S
import Data.OpenApi.ParamSchema (ToParamSchema (..))
import Data.Proxy
import Data.Schema
import Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))

newtype Proof = Proof {unProof :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString)

instance ToHttpApiData Proof where
  toQueryParam = decodeUtf8With lenientDecode . toByteString'

instance FromHttpApiData Proof where
  parseQueryParam =
    maybe (Left "Invalid Proof") Right
      . fromByteString'
      . fromStrict
      . encodeUtf8

instance ToParamSchema Proof where
  toParamSchema _ = toParamSchema (Proxy @Text)

newtype DPoPAccessToken = DPoPAccessToken {unDPoPAccessToken :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema DPoPAccessToken)

instance ToSchema DPoPAccessToken where
  schema = (decodeUtf8 . unDPoPAccessToken) .= fmap (DPoPAccessToken . encodeUtf8) (text "DPoPAccessToken")

instance ToParamSchema DPoPAccessToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToHttpApiData DPoPAccessToken where
  toQueryParam = decodeUtf8With lenientDecode . toByteString'

instance FromHttpApiData DPoPAccessToken where
  parseQueryParam =
    maybe (Left "Invalid DPoPAccessToken") Right
      . fromByteString'
      . fromStrict
      . encodeUtf8

data AccessTokenType = DPoP
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AccessTokenType)

instance ToSchema AccessTokenType where
  schema =
    enum @Text "AccessTokenType" $
      mconcat
        [ element "DPoP" DPoP
        ]

data DPoPAccessTokenResponse = DPoPAccessTokenResponse
  { datrToken :: DPoPAccessToken,
    datrType :: AccessTokenType,
    datrExpiresIn :: Word64
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema DPoPAccessTokenResponse)

instance ToSchema DPoPAccessTokenResponse where
  schema =
    object "DPoPAccessTokenResponse" $
      DPoPAccessTokenResponse
        <$> datrToken .= field "token" schema
        <*> datrType .= field "type" schema
        <*> datrExpiresIn .= field "expires_in" schema
