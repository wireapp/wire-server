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

module Wire.Sem.Paging.Cassandra
  ( CassandraPaging,
    LegacyPaging,
    InternalPaging,
    InternalPage (..),
    InternalPagingState (..),
    mkInternalPage,
    ipNext,
    ResultSet,
    mkResultSet,
    resultSetResult,
    resultSetType,
    ResultSetType (..),
  )
where

import Cassandra
import Data.Id
import Data.Qualified
import Data.Range
import Imports
import Wire.API.Connection (UserConnection)
import Wire.API.Team.Member (HardTruncationLimit, TeamMember)
import qualified Wire.Sem.Paging as E

-- | This paging system uses Cassandra's 'PagingState' to keep track of state,
-- and does not rely on ordering. This is the preferred way of paging across
-- multiple tables, as in  'MultiTablePaging'.
data CassandraPaging

type instance E.PagingState CassandraPaging a = PagingState

type instance E.Page CassandraPaging a = PageWithState a

type instance E.PagingBounds CassandraPaging ConvId = Range 1 1000 Int32

type instance E.PagingBounds CassandraPaging (Remote ConvId) = Range 1 1000 Int32

type instance E.PagingBounds CassandraPaging TeamId = Range 1 100 Int32

-- | This paging system is based on ordering, and keeps track of state using
-- the id of the next item to fetch. Implementations of this paging system also
-- contain extra logic to detect if the last page has been fetched.
data LegacyPaging

type instance E.PagingState LegacyPaging a = a

type instance E.Page LegacyPaging a = ResultSet a

type instance E.PagingBounds LegacyPaging ConvId = Range 1 1000 Int32

type instance E.PagingBounds LegacyPaging TeamId = Range 1 100 Int32

data InternalPaging

data InternalPagingState a = forall s. InternalPagingState (Page s, s -> Client a)

deriving instance (Functor InternalPagingState)

data InternalPage a = forall s. InternalPage (Page s, s -> Client a, [a])

deriving instance (Functor InternalPage)

mkInternalPage :: Page s -> (s -> Client a) -> Client (InternalPage a)
mkInternalPage p f = do
  items <- traverse f (result p)
  pure $ InternalPage (p, f, items)

ipNext :: InternalPagingState a -> Client (InternalPage a)
ipNext (InternalPagingState (p, f)) = do
  p' <- nextPage p
  mkInternalPage p' f

type instance E.PagingState InternalPaging a = InternalPagingState a

type instance E.Page InternalPaging a = InternalPage a

type instance E.PagingBounds InternalPaging TeamMember = Range 1 HardTruncationLimit Int32

type instance E.PagingBounds CassandraPaging TeamMember = Range 1 HardTruncationLimit Int32

type instance E.PagingBounds InternalPaging TeamId = Range 1 100 Int32

type instance E.PagingBounds InternalPaging (Remote UserConnection) = Range 1 1000 Int32

instance E.Paging InternalPaging where
  pageItems (InternalPage (_, _, items)) = items
  pageHasMore (InternalPage (p, _, _)) = hasMore p
  pageState (InternalPage (p, f, _)) = InternalPagingState (p, f)

-- We use this newtype to highlight the fact that the 'Page' wrapped in here
-- can not reliably used for paging.
--
-- The reason for this is that Cassandra returns 'hasMore' as true if the
-- page size requested is equal to result size. To work around this we
-- actually request for one additional element and drop the last value if
-- necessary. This means however that 'nextPage' does not work properly as
-- we would miss a value on every page size.
-- Thus, and since we don't want to expose the ResultSet constructor
-- because it gives access to `nextPage`, we give accessors to the results
-- and a more typed `hasMore` (ResultSetComplete | ResultSetTruncated)
data ResultSet a = ResultSet
  { resultSetResult :: [a],
    resultSetType :: ResultSetType
  }
  deriving stock (Show, Functor, Foldable, Traversable)

-- | A more descriptive type than using a simple bool to represent `hasMore`
data ResultSetType
  = ResultSetComplete
  | ResultSetTruncated
  deriving stock (Eq, Show)

mkResultSet :: Page a -> ResultSet a
mkResultSet page = ResultSet (result page) typ
  where
    typ
      | hasMore page = ResultSetTruncated
      | otherwise = ResultSetComplete
