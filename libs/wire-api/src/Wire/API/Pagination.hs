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
import Data.Range as Range
import Data.Schema
import Data.Text qualified as T
import GHC.Generics
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

------------------------------

newtype PageSize = PageSize {fromPageSize :: Range 1 500 Int32}
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PageSize

pageSizeToInt :: PageSize -> Int
pageSizeToInt = fromIntegral . pageSizeToInt32

pageSizeToInt32 :: PageSize -> Int32
pageSizeToInt32 = fromRange . fromPageSize

pageSizeFromInt :: Int32 -> Either Text PageSize
pageSizeFromInt = fmap PageSize . first T.pack . Range.checkedEither

-- | Doesn't crash on bad input, but shrinks it into the allowed range.
pageSizeFromIntUnsafe :: Int32 -> PageSize
pageSizeFromIntUnsafe = PageSize . unsafeRange . (+ 1) . (`mod` 500) . (+ (-1))

instance Arbitrary PageSize where
  arbitrary = pageSizeFromIntUnsafe <$> arbitrary

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

