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

module Wire.PaginationState
  ( PaginationState (..),
    paginationClause,
    mkPaginationState,
  )
where

import Data.Time.Clock
import Imports hiding (sortBy)
import Wire.API.Pagination
import Wire.Postgres

data PaginationState a
  = PaginationSortByName (Maybe (Text, a))
  | PaginationSortByCreatedAt (Maybe (UTCTime, a))

paginationClause :: (PostgresValue a) => PaginationState a -> Maybe Clause
paginationClause s = case s of
  PaginationSortByName (Just (name, x)) ->
    Just (mkClause "name" name <> mkClause "id" x)
  PaginationSortByCreatedAt (Just (createdAt, x)) ->
    Just (mkClause "created_at" createdAt <> mkClause "id" x)
  _ -> Nothing

mkPaginationState ::
  SortBy ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe a ->
  PaginationState a
mkPaginationState sortBy name createdAt x = case sortBy of
  SortByName -> PaginationSortByName $ (,) <$> name <*> x
  SortByCreatedAt -> PaginationSortByCreatedAt $ (,) <$> createdAt <*> x
