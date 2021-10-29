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

    -- * Simple paging
    SimplePaging,
  )
where

import Imports

type family Page p a :: (page :: *) | page -> p

type family PagingState p a = (ps :: *)

type family PagingBounds p a :: *

data SimplePaging

type instance Page SimplePaging a = [a]

type instance PagingState SimplePaging a = ()

type instance PagingBounds SimplePaging a = Int32
