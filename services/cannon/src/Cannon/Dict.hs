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
    { _map :: Vector (IORef (HashMap a b)) }

size :: MonadIO m => Dict a b -> m Int
size d = liftIO $ sum <$> mapM (\r -> M.size <$> readIORef r) (_map d)

empty :: MonadIO m => Int -> m (Dict a b)
empty w = liftIO $ if w > 0 && w < 8192
    then Dict <$> V.generateM w (const $ newIORef M.empty)
    else error "Dict.empty: slice number out of range [1, 8191]"

insert :: (Eq a, Hashable a, MonadIO m) => a -> b -> Dict a b -> m ()
insert k v = mutDict (M.insert k v) . getSlice k

add :: (Eq a, Hashable a, MonadIO m) => a -> b -> Dict a b -> m Bool
add k v d = liftIO $ atomicModifyIORef' (getSlice k d) $ \m ->
    if k `elem` M.keys m
        then (m, False)
        else (M.insert k v m, True)

remove :: (Eq a, Hashable a, MonadIO m) => a -> Dict a b -> m Bool
remove = removeIf (const True)

removeIf :: (Eq a, Hashable a, MonadIO m) => (Maybe b -> Bool) -> a -> Dict a b -> m Bool
removeIf f k d = liftIO $ atomicModifyIORef' (getSlice k d) $ \m ->
    if f (M.lookup k m)
        then (M.delete k m, True)
        else (m, False)

lookup :: (Eq a, Hashable a, MonadIO m) => a -> Dict a b -> m (Maybe b)
lookup k = liftIO . fmap (M.lookup k) . readIORef . getSlice k

-----------------------------------------------------------------------------
-- Internal

mutDict :: MonadIO m
        => (HashMap a b -> HashMap a b)
        -> IORef (HashMap a b)
        -> m ()
mutDict f d = liftIO $ atomicModifyIORef' d $ \m -> (f m, ())

getSlice :: (Hashable a) => a -> Dict a b -> IORef (HashMap a b)
getSlice k (Dict m) = m ! (hash k `mod` V.length m)
