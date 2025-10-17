-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Conversation.Pagination where

import Data.Aeson qualified as A
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import GHC.Generics
import Imports
import Wire.API.Conversation
import Wire.API.Pagination
import Wire.Arbitrary as Arbitrary

newtype ConversationPage = ConversationPage {page :: [ChannelSearchResult]}
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema ConversationPage

instance ToSchema ConversationPage where
  schema =
    objectWithDocModifier "ConversationPage" addPageDocs $
      ConversationPage <$> page .= field "page" (array schema)

instance Arbitrary ConversationPage where
  arbitrary = ConversationPage <$> arbitrary

data ChannelSearchResult = ChannelSearchResult
  { convId :: ConvId,
    name :: Maybe Text,
    access :: [Access],
    memberCount :: Int,
    adminCount :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ChannelSearchResult
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema ChannelSearchResult

instance ToSchema ChannelSearchResult where
  schema =
    object "ChannelSearchResult" $
      ChannelSearchResult
        <$> (.convId) .= field "id" schema
        <*> (.name) .= maybe_ (optField "name" schema)
        <*> (.access) .= field "access" (array schema)
        <*> (.memberCount) .= field "member_count" schema
        <*> (.adminCount) .= field "admin_count" schema
