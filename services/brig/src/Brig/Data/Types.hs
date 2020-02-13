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
