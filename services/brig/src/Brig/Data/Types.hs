-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Data.Types
  ( ResultPage,
    resultList,
    resultHasMore,

    -- * Constructors
    cassandraResultPage,
  )
where

import qualified Cassandra
import Imports

-- | An opaque page of results with an indication of whether
-- more data than contained in the page is available.
newtype ResultPage a = ResultPage (Cassandra.Page a)

resultList :: ResultPage a -> [a]
resultList (ResultPage p) = Cassandra.result p
{-# INLINE resultList #-}

resultHasMore :: ResultPage a -> Bool
resultHasMore (ResultPage p) = Cassandra.hasMore p
{-# INLINE resultHasMore #-}

cassandraResultPage :: Cassandra.Page a -> ResultPage a
cassandraResultPage = ResultPage
{-# INLINE cassandraResultPage #-}
