{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Metrics
    ( Path
    , Metrics
    , P.Counter
    , P.Gauge
    , Buckets

    , path
    , metrics

    , counterGet
    , counterAdd
    , counterIncr
    , counterValue

    , gaugeGet
    , gaugeAdd
    , gaugeSub
    , gaugeIncr
    , gaugeDecr
    , gaugeSet
    , gaugeValue

    , bucketsGet
    , bucketsIncr

    , render
    ) where

import Imports hiding (lookup, union)
import Data.Aeson
import Data.Hashable
import Data.Metrics.Buckets (Buckets)

import qualified Data.Text            as T
import qualified Data.HashMap.Strict  as Map
import qualified Data.Metrics.Buckets as Buckets
import qualified Data.Metrics.GC      as GC

import qualified Prometheus as P

newtype Path = Path { _path :: Text } deriving (Eq, Hashable, Semigroup, Monoid)

path :: Text -> Path
path = Path

data Metrics = Metrics
    { counters :: IORef (HashMap Path P.Counter)
    , gauges   :: IORef (HashMap Path P.Gauge)
    , buckets  :: IORef (HashMap Path Buckets)
    }

metrics :: MonadIO m => m Metrics
metrics = liftIO $ Metrics
    <$> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty

-- | Converts a CollectD style 'path' to a Metric name usable by prometheus
--   This is to provide back compatibility with the previous collect-d metric names
--   which often had paths and dot-separated names.
--
-- Currently just replaces all "/" and "." with "_" and lowercases the result
toInfo :: Path -> P.Info
toInfo (Path p) = P.Info (p & T.replace "." "_"
                            & T.replace "/" "_"
                            & T.toLower)
                        "description not provided"


getOrCreate :: (MonadIO m, Eq k, Hashable k) => IORef (HashMap k v) -> k -> IO v -> m v
getOrCreate mapRef key initializer = liftIO $ do
    hMap <- readIORef mapRef
    maybe initialize return (Map.lookup key hMap)
  where
    initialize = do
        val <- initializer
        atomicModifyIORef' mapRef $ \m -> (Map.insert key val m, val)

-----------------------------------------------------------------------------
-- P.Counter specifics

newCounter :: Path -> IO P.Counter
newCounter p = P.register $ P.counter (toInfo p)

counterGet :: MonadIO m => Path -> Metrics -> m P.Counter
counterGet p m = getOrCreate (counters m) p (newCounter p)

counterAdd :: MonadIO m => Double -> Path -> Metrics -> m ()
counterAdd x p m = liftIO $ do
    c <- counterGet p m
    void $ P.addCounter c x

counterIncr :: MonadIO m => Path -> Metrics -> m ()
counterIncr = counterAdd 1

counterValue :: MonadIO m => P.Counter -> m Double
counterValue c = P.getCounter c

-----------------------------------------------------------------------------
-- P.Gauge specifics

newGauge :: Path -> IO P.Gauge
newGauge p = P.register $ P.gauge (toInfo p )

gaugeGet :: MonadIO m => Path -> Metrics -> m P.Gauge
gaugeGet p m = getOrCreate (gauges m) p (newGauge p)

gaugeSet :: MonadIO m => Double -> Path -> Metrics -> m ()
gaugeSet x p m = liftIO $ do
    g <- gaugeGet p m
    P.setGauge g x

gaugeAdd :: MonadIO m => Double -> Path -> Metrics -> m ()
gaugeAdd x p m = liftIO $ do
    g <- gaugeGet p m
    P.addGauge g x

gaugeIncr :: MonadIO m => Path -> Metrics -> m ()
gaugeIncr = gaugeAdd 1

gaugeDecr :: MonadIO m => Path -> Metrics -> m ()
gaugeDecr = gaugeAdd (-1)

gaugeSub :: MonadIO m => Double -> Path -> Metrics -> m ()
gaugeSub x = gaugeAdd (-x)

gaugeValue :: MonadIO m => P.Gauge -> m Double
gaugeValue g = liftIO $ P.getGauge g

-----------------------------------------------------------------------------
-- Buckets specifics

bucketsGet :: MonadIO m => Int -> Int -> Path -> Metrics -> m Buckets
bucketsGet k n t m = liftIO $ do
    bs <- readIORef (buckets m)
    maybe make return (Map.lookup t bs)
  where
    make = do
        b <- Buckets.create k n
        atomicModifyIORef' (buckets m) $ \bs -> (Map.insert t b bs, b)

bucketsIncr :: MonadIO m => Int -> Int -> Word -> Path -> Metrics -> m ()
bucketsIncr k n x p m = liftIO $ do
    b <- bucketsGet k n p m
    Buckets.incr b x

-----------------------------------------------------------------------------
-- JSON rendering

class Jsonable a where
    toJson :: a -> IO Value

instance Jsonable P.Counter where
    toJson c = toJSON <$> counterValue c

instance Jsonable P.Gauge where
    toJson g = toJSON <$> gaugeValue g

instance Jsonable Buckets where
    toJson = Buckets.toJson

render :: MonadIO m => Metrics -> m Value
render m = liftIO $ do
    c <- snapshot =<< readIORef (counters m)
    g <- snapshot =<< readIORef (gauges m)
    b <- snapshot =<< readIORef (buckets m)
    h <- GC.toJson
    let result = c `union` g `union` b
    return $ maybe result (union result) h
  where
    snapshot :: Jsonable a => HashMap Path a -> IO Value
    snapshot = fmap object . mapM (\(k, v) -> (_path k .=) <$> toJson v) . Map.toList

    union :: Value -> Value -> Value
    union (Object a) (Object b) = Object $ a `merge` b
    union (Array  a) (Array  b) = Array  $ a <> b
    union Null       b          = b
    union a          _          = a

merge :: Object -> Object -> Object
merge a = expand (expand mempty a)
  where
    expand :: Object -> Object -> Object
    expand = Map.foldrWithKey (\k v obj -> insert obj (T.splitOn "." k) v)

    insert :: Object -> [Text] -> Value -> Object
    insert obj [t]    v = Map.insert t v obj
    insert obj (t:tt) v = Map.insert t (Object $ insert (subtree t obj) tt v) obj
    insert obj []     _ = obj

    subtree :: Text -> Object -> Object
    subtree t o = case Map.lookup t o of
        Just (Object x) -> x
        _               -> mempty
