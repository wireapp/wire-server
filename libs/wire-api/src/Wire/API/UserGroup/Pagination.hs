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

module Wire.API.UserGroup.Pagination where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LB
import Data.Default
import Data.OpenApi qualified as S
import Data.OpenApi.ParamSchema qualified as O
import Data.Proxy
import Data.Range as Range
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import Imports
import Numeric.Natural
import Servant.API
import Test.QuickCheck.Gen as Arbitrary
import Wire.API.UserGroup
import Wire.Arbitrary as Arbitrary

-- | Servant combinator for a pagination query.  Actually, it's not merely pagination, but
-- also sorting (and possibly filtering, who knows?).  Please generalize when needed
-- elsewhere.
--
-- Prior art: https://github.com/chordify/haskell-servant-pagination/
type PaginationQuery =
  QueryParam' '[Optional, Strict, Description "Search string"] "q" Text
    :> QueryParam' '[Optional, Strict] "sort_by" SortBy
    :> QueryParam' '[Optional, Strict] "sort_order" SortOrder
    :> QueryParam' '[Optional, Strict] "page_size" PageSize
    :> QueryParam' '[Optional, Strict] "offset" Natural -- use offset query param if you want to jump right to page 10 million.
    :> QueryParam'
         '[Optional, Strict, Description "Pagination state from last response (opaque to clients)"]
         "pagination_state"
         PaginationState
    :> Get '[JSON] PaginationResult

------------------------------

data SortBy = SortByName | SortByCreatedAt
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortBy

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

instance O.ToParamSchema SortBy -- TODO: what does swagger look like here?

------------------------------

data SortOrder = Asc | Desc
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortOrder

instance Arbitrary SortOrder where
  arbitrary = Arbitrary.elements [minBound ..]

defaultSortOrder :: SortBy -> SortOrder
defaultSortOrder SortByName = Asc
defaultSortOrder SortByCreatedAt = Desc

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

instance O.ToParamSchema SortOrder -- TODO: what does swagger look like here?

------------------------------

newtype PageSize = PageSize {fromPageSize :: Range 1 500 Int}
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PageSize

pageSizeToInt :: PageSize -> Int
pageSizeToInt = fromRange . fromPageSize

pageSizeFromInt :: Int -> Either Text PageSize
pageSizeFromInt = fmap PageSize . first T.pack . (Range.checkedEither @Int @1 @500 :: Int -> Either String (Range 1 500 Int))

-- | Doesn't crash on bad input, but shrinks it into the allowed range.
pageSizeFromIntUnsafe :: Int -> PageSize
pageSizeFromIntUnsafe = PageSize . (unsafeRange @Int @1 @500) . (+ 1) . (`mod` 500) . (+ (-1))

instance Arbitrary PageSize where
  arbitrary = pageSizeFromIntUnsafe <$> arbitrary

instance ToSchema PageSize where
  schema = PageSize <$> fromPageSize .= schema

instance FromHttpApiData PageSize where
  parseUrlPiece = parseUrlPiece @Int >=> pageSizeFromInt

instance O.ToParamSchema PageSize

instance Default PageSize where
  def = PageSize (unsafeRange 15)

------------------------------

-- | Offset-based pagination.
--
-- FUTUREWORK: For cursor-based pagination, there is postgres machinery.  It requires
-- transaction handling, but this may not imply table locks, so it may be fine.  (Jumping to
-- the last page may not be that relevant in practice, refining the search is more common.)
data PaginationState = PaginationState
  { -- | `searchString` always applies to name, no matter what the sort order.  `Nothing`
    -- means do not filter.
    searchString :: Maybe Text,
    sortBy :: SortBy,
    sortOrderName :: SortOrder,
    sortOrderCreatedAt :: SortOrder,
    pageSize :: PageSize,
    -- | Next page starts at the `offset`th row.
    offset :: Natural
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PaginationState

instance Default PaginationState where
  def =
    PaginationState
      { searchString = Nothing,
        sortBy = def,
        sortOrderName = defaultSortOrder SortByName,
        sortOrderCreatedAt = defaultSortOrder SortByCreatedAt,
        pageSize = def,
        offset = 0
      }

instance ToSchema PaginationState where
  schema =
    object "PagintationStatePayload" $
      PaginationState
        <$> (.searchString) .= maybe_ (optField "search_string" schema)
        <*> (.sortBy) .= field "sort_by" schema
        <*> (.sortOrderName) .= field "sort_order_by_name" schema
        <*> (.sortOrderCreatedAt) .= field "sort_order_by_created_at" schema
        <*> (.pageSize) .= field "page_size" schema
        <*> (.offset) .= field "offset" schema

instance Arbitrary PaginationState where
  arbitrary = PaginationState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance FromHttpApiData PaginationState where
  parseUrlPiece = parseUrlPieceViaSchema

instance O.ToParamSchema PaginationState where
  toParamSchema _ =
    -- PaginationState is supposed to be opaque for clients, no need to swagger docs.
    O.toParamSchema (Proxy @Text)

------------------------------

data PaginationResult = PaginationResult
  { page :: [UserGroup],
    state :: PaginationState
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PaginationResult

instance ToSchema PaginationResult where
  schema =
    objectWithDocModifier "PagintationResult" docs $
      PaginationResult
        <$> page .= field "page" (array schema)
        <*> state .= field "state" schema
    where
      docs :: NamedSwaggerDoc -> NamedSwaggerDoc
      docs =
        description
          ?~ "This is the last page iff it contains fewer rows than requested. There \
             \may return 0 rows on a page."

instance Arbitrary PaginationResult where
  arbitrary = PaginationResult <$> arbitrary <*> arbitrary

------------------------------

parseUrlPieceViaSchema :: (A.FromJSON a) => Text -> Either Text a
parseUrlPieceViaSchema = first T.pack . A.eitherDecode . LB.fromStrict . T.encodeUtf8
