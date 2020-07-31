{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

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
module Wire.API.Conversation.Code
  ( -- * ConversationCode
    ConversationCode (..),
    mkConversationCode,

    -- * re-exports
    Code.Key (..),
    Value (..),

    -- * Swagger
    modelConversationCode,
  )
where

import Control.Lens ((.~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), (.=))
import qualified Data.Aeson as JSON
import Data.ByteString.Conversion (toByteString')
-- FUTUREWORK: move content of Data.Code here?
import Data.Code as Code
import Data.Json.Util ((#))
import Data.Misc (HttpsUrl (HttpsUrl))
import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified URI.ByteString as URI
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

data ConversationCode = ConversationCode
  { conversationKey :: Code.Key,
    conversationCode :: Code.Value,
    conversationUri :: Maybe HttpsUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationCode)

modelConversationCode :: Doc.Model
modelConversationCode = Doc.defineModel "ConversationCode" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "key" Doc.string' $
    Doc.description "Stable conversation identifier"
  Doc.property "code" Doc.string' $
    Doc.description "Conversation code (random)"
  Doc.property "uri" Doc.string' $ do
    Doc.description "Full URI (containing key/code) to join a conversation"
    Doc.optional

instance ToJSON ConversationCode where
  toJSON j =
    JSON.object $
      "key" .= conversationKey j
        # "code" .= conversationCode j
        # "uri" .= conversationUri j
        # []

instance FromJSON ConversationCode where
  parseJSON = JSON.withObject "join" $ \o ->
    ConversationCode
      <$> o .: "key"
      <*> o .: "code"
      <*> o .:? "uri"

mkConversationCode :: Code.Key -> Code.Value -> HttpsUrl -> ConversationCode
mkConversationCode k v (HttpsUrl prefix) =
  ConversationCode
    { conversationKey = k,
      conversationCode = v,
      conversationUri = Just (HttpsUrl link)
    }
  where
    q = [("key", toByteString' k), ("code", toByteString' v)]
    link = prefix & (URI.queryL . URI.queryPairsL) .~ q
