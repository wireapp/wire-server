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
  ( -- * re-exports
    Key (..),
    Value (..),
    Timeout (..),
    KeyValuePair (..),

    -- * content
    ConversationCode (..),
    mkConversationCode,
  )
where

import Control.Lens ((.~))
import Data.Aeson ((.:), (.:?), (.=), FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as JSON
import Data.ByteString.Conversion (toByteString')
-- FUTUREWORK: move content of Data.Code here?
import Data.Code as Code
import Data.Json.Util ((#))
import Data.Misc (HttpsUrl (HttpsUrl))
import Imports
import qualified URI.ByteString as URI

data ConversationCode = ConversationCode
  { conversationKey :: !Code.Key,
    conversationCode :: !Code.Value,
    conversationUri :: !(Maybe HttpsUrl)
  }
  deriving (Eq, Show, Generic)

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

instance ToJSON ConversationCode where
  toJSON j =
    JSON.object $
      "key" .= conversationKey j
        # "code" .= conversationCode j
        # "uri" .= conversationUri j
        # []

instance FromJSON ConversationCode where
  parseJSON = JSON.withObject "join" $ \o ->
    ConversationCode <$> o .: "key"
      <*> o .: "code"
      <*> o .:? "uri"
