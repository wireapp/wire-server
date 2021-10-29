-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Cassandra.Paging
  ( CassandraPaging,
    LegacyPaging,
  )
where

import Cassandra
import Galley.Data.ResultSet
import qualified Galley.Effects.Paging as E

-- | This paging system uses Cassandra's 'PagingState' to keep track of state,
-- and does not rely on ordering. This is the preferred way of paging across
-- multiple tables, as in  'MultiTablePaging'.
data CassandraPaging

type instance E.PagingState CassandraPaging a = PagingState

type instance E.Page CassandraPaging a = PageWithState a

-- | This paging system is based on ordering, and keeps track of state using
-- the id of the next item to fetch. Implementations of this paging system also
-- contain extra logic to detect if the last page has been fetched.
data LegacyPaging

type instance E.PagingState LegacyPaging a = a

type instance E.Page LegacyPaging a = ResultSet a
