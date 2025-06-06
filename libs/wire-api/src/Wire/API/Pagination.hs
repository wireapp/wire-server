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
import Data.Default
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import GHC.Generics
import GHC.TypeLits
import Imports
import Servant.API

-- | (Is there an elegant way to enforce `allowedKeyFieldsInfo` before the handler kicks in?)
type PaginationQuery (allowedKeyFieldsInfo :: Symbol) api =
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
              )
          ]
         "sortBy"
         [Text]
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
         PaginationState
    :> api

data SortOrder = Asc | Desc
  deriving (Eq, Show, Ord, Enum, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortOrder

instance ToSchema SortOrder where
  schema =
    enum @Text "SortOrder" $
      mconcat
        [ element "asc" Asc,
          element "desc" Desc
        ]

newtype PageSize = PageSize {fromPageSize :: Range 1 500 Int}
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PageSize

instance ToSchema PageSize where
  schema = PageSize <$> fromPageSize .= schema

instance Default PageSize where
  def = PageSize (unsafeRange 15)

data PaginationState = PaginationState
  { searchString :: Text,
    sortBy :: [Text],
    sortOrder :: SortOrder,
    pageSize :: PageSize,
    lastRowSent :: [Text]
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PaginationState

instance ToSchema PaginationState where
  schema =
    object "PagintationState" $
      PaginationState
        <$> (.searchString) .= field "search_string" schema
        <*> (.sortBy) .= field "sort_by" (array schema)
        <*> (.sortOrder) .= field "sort_order" schema
        <*> (.pageSize) .= field "page_size" schema
        <*> (.lastRowSent) .= field "last_row_sent" (array schema)

data PaginationResult a = PaginationResult
  { page :: [a],
    state :: PaginationState
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema (PaginationResult a)

instance (ToSchema a) => ToSchema (PaginationResult a) where
  schema =
    object "PagintationResult" $
      PaginationResult
        <$> page .= field "page" (array schema)
        <*> state .= field "state" schema
