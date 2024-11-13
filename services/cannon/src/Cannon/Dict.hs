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

module Cannon.Dict
  ( Dict,
    empty,
    add,
    insert,
    remove,
    removeIf,
    lookup,
    size,
    toList,
    keys,
  )
where

import Data.Hashable (Hashable, hash)
import Data.SizedHashMap (SizedHashMap)
import Data.SizedHashMap qualified as SHM
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Imports hiding (lookup, toList)

newtype Dict a b = Dict
  { _map :: Vector (IORef (SizedHashMap a b))
  }

keys :: (MonadIO m) => Dict b v -> m [b]
keys d = toList d <&> map fst

size :: (MonadIO m) => Dict a b -> m Int
size d = liftIO $ sum <$> mapM (fmap SHM.size . readIORef) (_map d)

empty :: (MonadIO m) => Int -> m (Dict a b)
empty w =
  liftIO $
    if w > 0 && w < 8192
      then Dict <$> V.generateM w (const $ newIORef SHM.empty)
      else error "Dict.empty: slice number out of range [1, 8191]"

insert :: (Hashable a, MonadIO m) => a -> b -> Dict a b -> m ()
insert k v = mutDict (SHM.insert k v) . getSlice k

add :: (Hashable a, MonadIO m) => a -> b -> Dict a b -> m Bool
add k v d = liftIO . atomicModifyIORef' (getSlice k d) $ \m ->
  if k `elem` SHM.keys m
    then (m, False)
    else (SHM.insert k v m, True)

remove :: (Hashable a, MonadIO m) => a -> Dict a b -> m Bool
remove = removeIf (const True)

removeIf :: (Hashable a, MonadIO m) => (Maybe b -> Bool) -> a -> Dict a b -> m Bool
removeIf f k d = liftIO . atomicModifyIORef' (getSlice k d) $ \m ->
  if f (SHM.lookup k m)
    then (SHM.delete k m, True)
    else (m, False)

lookup :: (Hashable a, MonadIO m) => a -> Dict a b -> m (Maybe b)
lookup k = liftIO . fmap (SHM.lookup k) . readIORef . getSlice k

toList :: (MonadIO m) => Dict a b -> m [(a, b)]
toList =
  fmap (mconcat . V.toList)
    . V.mapM (fmap SHM.toList . readIORef)
    . _map

-----------------------------------------------------------------------------
-- Internal

mutDict ::
  (MonadIO m) =>
  (SizedHashMap a b -> SizedHashMap a b) ->
  IORef (SizedHashMap a b) ->
  m ()
mutDict f d = liftIO . atomicModifyIORef' d $ \m -> (f m, ())

getSlice :: (Hashable a) => a -> Dict a b -> IORef (SizedHashMap a b)
getSlice k (Dict m) = m ! (hash k `mod` V.length m)
