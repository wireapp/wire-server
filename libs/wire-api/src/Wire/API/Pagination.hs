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

import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LB
import Data.Default
import Data.Kind
import Data.OpenApi qualified as S
import Data.OpenApi.ParamSchema qualified as O
import Data.Proxy
import Data.Range
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import GHC.TypeLits
import Imports
import Servant.API
import Test.QuickCheck.Gen as Arbitrary
import Wire.Arbitrary as Arbitrary

-- | (Is there an elegant way to enforce `allowedKeyFieldsInfo` before the handler kicks in?)
type PaginationQuery (allowedKeyFieldsInfo :: Symbol) (rowKeys :: Type) (row :: Type) =
  QueryParam'
    '[Optional, Strict, Description "Search string"]
    "q"
    Text
    :> QueryParam'
         '[ Optional,
            Strict,
            Description
              ( "Sort key(s): comma-separated list of field names.  Must \
                \match sort keys encoded in pagination state.  Allowed Fields: "
                  `AppendSymbol` allowedKeyFieldsInfo
                  -- we need `allowedKeyFieldInfo` because `SortBy` contains that information only on the value level.
              )
          ]
         "sortBy"
         SortBy
    :> QueryParam'
         '[Optional, Strict, Description "Sort order"]
         "sortOrder"
         SortOrder
    :> QueryParam'
         '[Optional, Strict, Description "Page size"]
         "pageSize"
         PageSize
    :> QueryParam'
         '[Optional, Strict, Description "Pagination state from last response (opaque to clients)"]
         "paginationState"
         (PaginationState rowKeys)
    :> Get '[JSON] (PaginationResult rowKeys row)

data SortBy = SortBy {fromSortBy :: [Text]}
  deriving (Eq, Ord, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortBy

instance Arbitrary SortBy where
  arbitrary = SortBy <$> arbitrary

instance ToSchema SortBy where
  schema = object "SortBy" $ SortBy <$> (.fromSortBy) .= field "keys" (array schema)

instance FromHttpApiData SortBy where
  parseUrlPiece = parseUrlPieceViaSchema

instance O.ToParamSchema SortBy

data SortOrder = Asc | Desc
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortOrder

instance Arbitrary SortOrder where
  arbitrary = Arbitrary.elements [minBound ..]

instance Default SortOrder where
  def = Desc

instance ToSchema SortOrder where
  schema =
    enum @Text "SortOrder" $
      mconcat
        [ element "asc" Asc,
          element "desc" Desc
        ]

instance FromHttpApiData SortOrder where
  parseUrlPiece = parseUrlPieceViaSchema

instance O.ToParamSchema SortOrder

newtype PageSize = PageSize {fromPageSize :: Range 1 500 Int}
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PageSize

pageSizeToInt :: PageSize -> Int
pageSizeToInt = fromRange . fromPageSize

-- | Doesn't crash on bad input, but shrinks it into the allowed range.
pageSizeFromIntUnsafe :: Int -> PageSize
pageSizeFromIntUnsafe = PageSize . (unsafeRange @Int @1 @500) . (+ 1) . (`mod` 500)

instance Arbitrary PageSize where
  arbitrary = pageSizeFromIntUnsafe <$> arbitrary

instance ToSchema PageSize where
  schema = PageSize <$> fromPageSize .= schema

instance FromHttpApiData PageSize where
  parseUrlPiece = parseUrlPieceViaSchema

instance O.ToParamSchema PageSize

instance Default PageSize where
  def = PageSize (unsafeRange 15)

data PaginationState key = PaginationState
  { searchString :: Text, -- TODO: this shouldn't be in the state, but maintained separately. or we need to make it more polymorhpic, maybe?
    sortByKeys :: SortBy,
    sortOrder :: SortOrder,
    pageSize :: PageSize,
    lastRowSent :: key
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema (PaginationState key)

instance (ToSchema key) => ToSchema (PaginationState key) where
  schema =
    object "PagintationState" $
      PaginationState
        <$> (.searchString) .= field "search_string" schema
        <*> (.sortByKeys) .= field "sort_by" schema
        <*> (.sortOrder) .= field "sort_order" schema
        <*> (.pageSize) .= field "page_size" schema
        <*> (.lastRowSent) .= field "last_row_sent" schema

instance (Arbitrary key) => Arbitrary (PaginationState key) where
  arbitrary = PaginationState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (ToSchema key) => FromHttpApiData (PaginationState key) where
  parseUrlPiece = parseUrlPieceViaSchema

instance O.ToParamSchema (PaginationState key) where
  toParamSchema _ =
    -- PaginationState is supposed to be opaque for clients, no need to swagger docs.
    O.toParamSchema (Proxy @Text)

data PaginationResult key row = PaginationResult
  { page :: [row],
    state :: PaginationState key
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema (PaginationResult key row)

instance (ToSchema key, ToSchema row) => ToSchema (PaginationResult key row) where
  schema =
    object "PagintationResult" $
      PaginationResult
        <$> page .= field "page" (array schema)
        <*> state .= field "state" schema

instance (Arbitrary key, Arbitrary row) => Arbitrary (PaginationResult key row) where
  arbitrary = PaginationResult <$> arbitrary <*> arbitrary

parseUrlPieceViaSchema :: (A.FromJSON a) => Text -> Either Text a
parseUrlPieceViaSchema = first T.pack . A.eitherDecode . LB.fromStrict . T.encodeUtf8
