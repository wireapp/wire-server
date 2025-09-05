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
{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Conversation.Pagination where

import Control.Lens (makePrisms, (?~))
import Data.Aeson qualified as A
import Data.Default
import Data.OpenApi qualified as S
import Data.Schema
import GHC.Generics
import Imports
import Servant.API
import Test.QuickCheck.Gen as Arbitrary
import Wire.API.Conversation
import Wire.API.Pagination
import Wire.Arbitrary as Arbitrary

newtype ConversationPage = ConversationPage {page :: [Conversation]}
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema ConversationPage

instance ToSchema ConversationPage where
  schema =
    objectWithDocModifier "ConversationPage" docs $
      ConversationPage <$> page .= field "page" (array schema)
    where
      docs :: NamedSwaggerDoc -> NamedSwaggerDoc
      docs =
        description
          ?~ "This is the last page if it contains fewer rows than requested. There \
             \may be 0 rows on a page."

instance Arbitrary ConversationPage where
  arbitrary = ConversationPage <$> arbitrary

------------------------------

data SortBy = SortByName | SortByCreatedAt
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortBy

sortColumnName :: SortBy -> Text
sortColumnName = \case
  SortByName -> "name"
  SortByCreatedAt -> "created_at"

instance Default SortBy where
  def = SortByCreatedAt

instance ToSchema SortBy where
  schema =
    enum @Text "SortBy" $
      mconcat
        [ element "name" SortByName,
          element "created_at" SortByCreatedAt
        ]

instance Arbitrary SortBy where
  arbitrary = Arbitrary.elements [minBound ..]

instance FromHttpApiData SortBy where
  parseUrlPiece "name" = pure SortByName
  parseUrlPiece "created_at" = pure SortByCreatedAt
  parseUrlPiece bad = Left $ "SortBy: could not parse " <> bad

instance S.ToParamSchema SortBy where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_ ?~ ["name", "created_at"]

------------------------------

defaultSortOrder :: SortBy -> SortOrder
defaultSortOrder SortByName = Asc
defaultSortOrder SortByCreatedAt = Desc

makePrisms ''ConversationPage
