{-# LANGUAGE StrictData #-}

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

-- | Types for verification codes.
module Wire.API.Conversation.Code
  ( -- * ConversationCode
    ConversationCode (..),
    mkConversationCode,
    CreateConversationCodeRequest (..),

    -- * re-exports
    Code.Key (..),
    Value (..),
  )
where

import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Conversion (toByteString')
-- FUTUREWORK: move content of Data.Code here?
import Data.Code as Code
import Data.Misc
import Data.Schema
import qualified Data.Swagger as S
import Imports
import qualified URI.ByteString as URI
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

newtype CreateConversationCodeRequest = CreateConversationCodeRequest
  { cccrPassword :: Maybe PlainTextPassword8
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CreateConversationCodeRequest

instance ToSchema CreateConversationCodeRequest where
  schema =
    objectWithDocModifier
      "CreateConversationCodeRequest"
      (description ?~ "Optional request body for creating a conversation code with a password")
      $ CreateConversationCodeRequest <$> cccrPassword .= maybe_ (optField "password" schema)

data ConversationCode = ConversationCode
  { conversationKey :: Code.Key,
    conversationCode :: Code.Value,
    conversationUri :: Maybe HttpsUrl,
    conversationHasPassword :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationCode)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationCode

instance ToSchema ConversationCode where
  schema =
    objectWithDocModifier
      "ConversationCode"
      (description ?~ "Contains conversation properties to update")
      $ ConversationCode
        <$> conversationKey
          .= fieldWithDocModifier
            "key"
            (description ?~ "Stable conversation identifier")
            schema
        <*> conversationCode
          .= fieldWithDocModifier
            "code"
            (description ?~ "Conversation code (random)")
            schema
        <*> conversationUri
          .= maybe_
            ( optFieldWithDocModifier
                "uri"
                (description ?~ "Full URI (containing key/code) to join a conversation")
                schema
            )
        <*> conversationHasPassword .= maybe_ (optField "has_password" schema)

mkConversationCode :: Code.Key -> Code.Value -> Bool -> HttpsUrl -> ConversationCode
mkConversationCode k v hasPw (HttpsUrl prefix) =
  ConversationCode
    { conversationKey = k,
      conversationCode = v,
      conversationUri = Just (HttpsUrl link),
      conversationHasPassword = Just hasPw
    }
  where
    q = [("key", toByteString' k), ("code", toByteString' v)]
    link = prefix & (URI.queryL . URI.queryPairsL) .~ q
