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

module Wire.API.Pagination where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.Default
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Range as Range
import Data.Schema
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeNats
import Imports
import Servant.API
import Test.QuickCheck.Gen as Arbitrary
import Wire.Arbitrary as Arbitrary

data SortOrder = Asc | Desc
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortOrder

sortOrderClause :: SortOrder -> Text
sortOrderClause = \case
  Asc -> "asc"
  Desc -> "desc"

sortOrderOperator :: SortOrder -> Text
sortOrderOperator = \case
  Asc -> ">"
  Desc -> "<"

instance Arbitrary SortOrder where
  arbitrary = Arbitrary.elements [minBound ..]

instance ToSchema SortOrder where
  schema =
    enum @Text "SortOrder" $
      mconcat
        [ element "asc" Asc,
          element "desc" Desc
        ]

instance FromHttpApiData SortOrder where
  parseUrlPiece "asc" = pure Asc
  parseUrlPiece "desc" = pure Desc
  parseUrlPiece bad = Left $ "SortOrder: could not parse " <> bad

instance S.ToParamSchema SortOrder where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_ ?~ ["asc", "desc"]

--------------------------------------------------------------------------------

newtype PageSize = PageSize {fromPageSize :: Range 0 MaxPageSize Word}
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PageSize

pageSizeToInt :: PageSize -> Int
pageSizeToInt = fromIntegral . pageSizeToWord

pageSizeToWord :: PageSize -> Word
pageSizeToWord = fromRange . fromPageSize

pageSizeFromInt :: Word -> Either Text PageSize
pageSizeFromInt = fmap PageSize . first T.pack . Range.checkedEither

type MaxPageSize = 500 :: Nat

maxPageSize :: (Num i) => i
maxPageSize = fromIntegral $ natVal (Proxy @MaxPageSize)

-- | Doesn't crash on bad input, but shrinks it into the allowed range.
pageSizeFromIntegralTotal :: (Integral i) => i -> PageSize
pageSizeFromIntegralTotal = PageSize . unsafeRange . fromIntegral . min maxPageSize . max 0

instance Arbitrary PageSize where
  arbitrary = pageSizeFromIntegralTotal <$> (arbitrary @Int)

instance ToSchema PageSize where
  schema = PageSize <$> fromPageSize .= schema

instance FromHttpApiData PageSize where
  parseUrlPiece = parseUrlPiece >=> pageSizeFromInt

instance S.ToParamSchema PageSize where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiNumber
      & S.description ?~ "integer from [1..500]"

instance Default PageSize where
  def = PageSize (unsafeRange 15)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

defaultSortOrder :: SortBy -> SortOrder
defaultSortOrder SortByName = Asc
defaultSortOrder SortByCreatedAt = Desc

--------------------------------------------------------------------------------

addPageDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
addPageDocs =
  description
    ?~ "This is the last page if it contains fewer rows than requested. There \
       \may be 0 rows on a page."
