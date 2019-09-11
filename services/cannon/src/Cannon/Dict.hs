module Cannon.Dict
    ( Dict
    , empty
    , add
    , insert
    , remove
    , removeIf
    , lookup
    , size
    )
where

import Imports hiding (lookup)
import Data.Hashable (hash, Hashable)
import Data.Vector (Vector, (!))

import qualified Data.HashMap.Strict as M
import qualified Data.Vector         as V

newtype Dict a b = Dict
    { _map :: Vector (IORef (SizedHashMap a b)) }

size :: MonadIO m => Dict a b -> m Int
size d = liftIO $ sum <$> mapM (\r -> hmsize <$> readIORef r) (_map d)

empty :: MonadIO m => Int -> m (Dict a b)
empty w = liftIO $ if w > 0 && w < 8192
    then Dict <$> V.generateM w (const $ newIORef hmempty)
    else error "Dict.empty: slice number out of range [1, 8191]"

insert :: (Eq a, Hashable a, MonadIO m) => a -> b -> Dict a b -> m ()
insert k v = mutDict (hminsert k v) . getSlice k

add :: (Eq a, Hashable a, MonadIO m) => a -> b -> Dict a b -> m Bool
add k v d = liftIO $ atomicModifyIORef' (getSlice k d) $ \m ->
    if k `elem` hmkeys m
        then (m, False)
        else (hminsert k v m, True)

remove :: (Eq a, Hashable a, MonadIO m) => a -> Dict a b -> m Bool
remove = removeIf (const True)

removeIf :: (Eq a, Hashable a, MonadIO m) => (Maybe b -> Bool) -> a -> Dict a b -> m Bool
removeIf f k d = liftIO $ atomicModifyIORef' (getSlice k d) $ \m ->
    if f (hmlookup k m)
        then (hmdelete k m, True)
        else (m, False)

lookup :: (Eq a, Hashable a, MonadIO m) => a -> Dict a b -> m (Maybe b)
lookup k = liftIO . fmap (hmlookup k) . readIORef . getSlice k

-----------------------------------------------------------------------------
-- Internal

mutDict :: MonadIO m
        => (SizedHashMap a b -> SizedHashMap a b)
        -> IORef (SizedHashMap a b)
        -> m ()
mutDict f d = liftIO $ atomicModifyIORef' d $ \m -> (f m, ())

getSlice :: (Hashable a) => a -> Dict a b -> IORef (SizedHashMap a b)
getSlice k (Dict m) = m ! (hash k `mod` V.length m)

----------------------------------------------------------------------
-- hashmap with O(1) size operator

data SizedHashMap a b = SizedHashMap !Int !(HashMap a b)

hmsize :: forall k v. SizedHashMap k v -> Int
hmsize (SizedHashMap s _) = s

hmempty :: forall k v. SizedHashMap k v
hmempty = SizedHashMap 0 M.empty

hminsert :: forall k v. (Eq k, Hashable k) => k -> v -> SizedHashMap k v -> SizedHashMap k v
hminsert k v (SizedHashMap n hm) = SizedHashMap n' hm'
  where
    n' = if M.member k hm then n else n + 1
    hm' = M.insert k v hm

hmkeys :: forall k v. SizedHashMap k v -> [k]
hmkeys (SizedHashMap _ hm) = M.keys hm

hmlookup :: forall k v. (Eq k, Hashable k) => k -> SizedHashMap k v -> Maybe v
hmlookup k (SizedHashMap _ hm) = M.lookup k hm

hmdelete :: forall k v. (Eq k, Hashable k) => k -> SizedHashMap k v -> SizedHashMap k v
hmdelete k (SizedHashMap n hm) = SizedHashMap n' hm'
  where
    n' = if M.member k hm then n - 1 else n
    hm' = M.delete k hm
