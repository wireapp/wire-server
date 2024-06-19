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

module Data.SizedHashMap
  ( SizedHashMap,
    fromSizedHashMap,
    size,
    empty,
    insert,
    keys,
    elems,
    toList,
    lookup,
    delete,
  )
where

import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Imports hiding (lookup, toList)

data SizedHashMap k v = SizedHashMap !Int !(HashMap k v)

fromSizedHashMap :: SizedHashMap k v -> HashMap k v
fromSizedHashMap (SizedHashMap _ hm) = hm

size :: forall k v. SizedHashMap k v -> Int
size (SizedHashMap s _) = s

empty :: forall k v. SizedHashMap k v
empty = SizedHashMap 0 M.empty

insert :: forall k v. (Hashable k) => k -> v -> SizedHashMap k v -> SizedHashMap k v
insert k v (SizedHashMap n hm) = SizedHashMap n' hm'
  where
    n' = if M.member k hm then n else n + 1
    hm' = M.insert k v hm

keys :: forall k v. SizedHashMap k v -> [k]
keys (SizedHashMap _ hm) = M.keys hm

elems :: forall k v. SizedHashMap k v -> [v]
elems (SizedHashMap _ hm) = M.elems hm

toList :: forall k v. SizedHashMap k v -> [(k, v)]
toList (SizedHashMap _ hm) = M.toList hm

lookup :: forall k v. (Hashable k) => k -> SizedHashMap k v -> Maybe v
lookup k (SizedHashMap _ hm) = M.lookup k hm

delete :: forall k v. (Hashable k) => k -> SizedHashMap k v -> SizedHashMap k v
delete k (SizedHashMap n hm) = SizedHashMap n' hm'
  where
    n' = if M.member k hm then n - 1 else n
    hm' = M.delete k hm
