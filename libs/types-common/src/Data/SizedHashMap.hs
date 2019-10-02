module Data.SizedHashMap
    ( SizedHashMap
    , fromSizedHashMap
    , size
    , empty
    , insert
    , keys
    , lookup
    , delete
    )
where

import Imports hiding (lookup)
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as M


data SizedHashMap k v = SizedHashMap !Int !(HashMap k v)

fromSizedHashMap :: SizedHashMap k v -> HashMap k v
fromSizedHashMap (SizedHashMap _ hm) = hm

size :: forall k v. SizedHashMap k v -> Int
size (SizedHashMap s _) = s

empty :: forall k v. SizedHashMap k v
empty = SizedHashMap 0 M.empty

insert :: forall k v. (Eq k, Hashable k) => k -> v -> SizedHashMap k v -> SizedHashMap k v
insert k v (SizedHashMap n hm) = SizedHashMap n' hm'
  where
    n' = if M.member k hm then n else n + 1
    hm' = M.insert k v hm

keys :: forall k v. SizedHashMap k v -> [k]
keys (SizedHashMap _ hm) = M.keys hm

lookup :: forall k v. (Eq k, Hashable k) => k -> SizedHashMap k v -> Maybe v
lookup k (SizedHashMap _ hm) = M.lookup k hm

delete :: forall k v. (Eq k, Hashable k) => k -> SizedHashMap k v -> SizedHashMap k v
delete k (SizedHashMap n hm) = SizedHashMap n' hm'
  where
    n' = if M.member k hm then n - 1 else n
    hm' = M.delete k hm
