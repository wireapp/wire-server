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

module Galley.Effects.Paging
  ( -- * General paging types
    Page,
    PagingState,
    PagingBounds,
    Paging (..),

    -- * Utilities
    withChunks,

    -- * Simple paging
    SimplePaging,
  )
where

import Imports

type family Page p a :: (page :: *) | page -> p a

type family PagingState p a = (ps :: *)

type family PagingBounds p a :: *

class Paging p where
  pageItems :: Page p a -> [a]
  pageHasMore :: Page p a -> Bool
  pageState :: Page p a -> PagingState p a

data SimplePaging

type instance Page SimplePaging a = [a]

type instance PagingState SimplePaging a = ()

type instance PagingBounds SimplePaging a = Int32

instance Paging SimplePaging where
  pageItems = id
  pageHasMore _ = False
  pageState _ = ()

withChunks ::
  (Paging p, Monad m) =>
  (Maybe (PagingState p i) -> m (Page p i)) ->
  ([i] -> m ()) ->
  m ()
withChunks pager action = do
  page <- pager Nothing
  go page
  where
    go page = do
      action (pageItems page)
      when (pageHasMore page) $ do
        page' <- pager (Just (pageState page))
        go page'
