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

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString.Char8 (split)
import Data.ByteString.Conversion (FromByteString (..), ToByteString, fromByteString', toByteString')
import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Data.Proxy (Proxy (Proxy))
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Swagger.ParamSchema
import Data.UUID (UUID)
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Wire.API.MLS.Epoch

newtype Proof = Proof {unProof :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString)

instance ToHttpApiData Proof where
  toQueryParam = cs . toByteString'

instance FromHttpApiData Proof where
  parseQueryParam = maybe (Left "Invalid Proof") Right . fromByteString' . cs

instance ToParamSchema Proof where
  toParamSchema _ = toParamSchema (Proxy @Text)

claims :: Proof -> Either String Claims
claims =
  claimsBs >=> (fmap cs <$> Base64.decode) >=> eitherDecode
  where
    claimsBs :: Proof -> Either String ByteString
    claimsBs (Proof bs) =
      case split '.' bs of
        [_, c, _] -> pure c
        _ : _ : _ : _ -> Left "expected 3 parts, but got more"
        _ -> Left "expected 3 parts, but got less"

data Claims = Claims
  { cJti :: UUID,
    cIat :: Epoch,
    cHtm :: Text,
    cHtu :: HttpsUrl,
    cNonce :: Nonce,
    cChal :: Text,
    cSub :: Text,
    cExp :: Epoch
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Claims)

instance ToSchema Claims where
  schema =
    object "Claims" $
      Claims
        <$> cJti .= field "jti" genericToSchema
        <*> cIat .= field "iat" schema
        <*> cHtm .= field "htm" schema
        <*> cHtu .= field "htu" schema
        <*> cNonce .= field "nonce" schema
        <*> cChal .= field "chal" schema
        <*> cSub .= field "sub" schema
        <*> cExp .= field "exp" schema

newtype DPoPAccessToken = DPoPAccessToken {unDPoPAccessToken :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString)

instance ToParamSchema DPoPAccessToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToHttpApiData DPoPAccessToken where
  toQueryParam = cs . toByteString'

instance FromHttpApiData DPoPAccessToken where
  parseQueryParam = maybe (Left "Invalid DPoPAccessToken") Right . fromByteString' . cs
