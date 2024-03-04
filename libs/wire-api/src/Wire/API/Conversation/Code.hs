{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    CreateConversationCodeRequest (..),
    JoinConversationByCode (..),
    ConversationCodeInfo (..),
    mkConversationCodeInfo,

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
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import URI.ByteString qualified as URI
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

newtype CreateConversationCodeRequest = CreateConversationCodeRequest
  { password :: Maybe PlainTextPassword8
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CreateConversationCodeRequest)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CreateConversationCodeRequest

instance ToSchema CreateConversationCodeRequest where
  schema :: ValueSchema NamedSwaggerDoc CreateConversationCodeRequest
  schema =
    objectWithDocModifier
      "CreateConversationCodeRequest"
      (description ?~ "Request body for creating a conversation code")
      $ CreateConversationCodeRequest
        <$> (.password) .= maybe_ (optFieldWithDocModifier "password" desc schema)
    where
      desc = description ?~ "Password for accessing the conversation via guest link. Set to null or omit for no password."

data JoinConversationByCode = JoinConversationByCode
  { code :: ConversationCode,
    password :: Maybe PlainTextPassword8
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform JoinConversationByCode)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema JoinConversationByCode

instance ToSchema JoinConversationByCode where
  schema =
    objectWithDocModifier
      "JoinConversationByCode"
      (description ?~ "Request body for joining a conversation by code")
      $ JoinConversationByCode
        <$> (.code) .= conversationCodeObjectSchema
        <*> (.password) .= maybe_ (optField "password" schema)

data ConversationCode = ConversationCode
  { conversationKey :: Code.Key,
    conversationCode :: Code.Value,
    conversationUri :: Maybe HttpsUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationCode)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationCode

conversationCodeObjectSchema :: ObjectSchema SwaggerDoc ConversationCode
conversationCodeObjectSchema =
  ConversationCode
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

instance ToSchema ConversationCode where
  schema =
    objectWithDocModifier
      "ConversationCode"
      (description ?~ "Contains conversation properties to update")
      conversationCodeObjectSchema

data ConversationCodeInfo = ConversationCodeInfo
  { code :: ConversationCode,
    hasPassword :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationCodeInfo)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationCodeInfo

instance ToSchema ConversationCodeInfo where
  schema =
    objectWithDocModifier
      "ConversationCodeInfo"
      (description ?~ "Contains conversation properties to update")
      $ ConversationCodeInfo
        <$> (.code) .= conversationCodeObjectSchema
        <*> (.hasPassword) .= fieldWithDocModifier "has_password" (description ?~ "Whether the conversation has a password") schema

mkConversationCodeInfo :: Bool -> Code.Key -> Code.Value -> HttpsUrl -> ConversationCodeInfo
mkConversationCodeInfo hasPw k v (HttpsUrl prefix) =
  ConversationCodeInfo (ConversationCode k v (Just (HttpsUrl link))) hasPw
  where
    q = [("key", toByteString' k), ("code", toByteString' v)]
    link = prefix & (URI.queryL . URI.queryPairsL) .~ q
