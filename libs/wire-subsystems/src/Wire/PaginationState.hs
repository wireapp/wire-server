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
