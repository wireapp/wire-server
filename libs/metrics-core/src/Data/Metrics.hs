{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Metrics
    ( Path
    , Metrics
    , Histogram
    , Counter
    , Gauge

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

    , HistogramInfo
    , Buckets
    , Bucket

    , linearHistogram
    , exponentialHistogram
    , customHistogram
    , deprecatedRequestDurationHistogram

    , histoGet
    , histoSubmit
    , histoValue
    , histoTimeAction

    , render
    ) where

import Imports hiding (lookup, union)
import Data.Aeson
import Data.Hashable

import qualified Data.Text            as T
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map.Strict      as M
import qualified Data.Metrics.GC      as GC

import qualified Prometheus as P

type Counter = P.Counter
type Gauge = P.Gauge
type Histogram = P.Histogram

newtype Path =
    Path
        { _path :: Text
        }
    deriving (Eq, Show, Hashable, Semigroup, Monoid)

path :: Text -> Path
path = Path

data Metrics =
    Metrics
        { counters   :: IORef (HashMap Path Counter)
        , gauges     :: IORef (HashMap Path Gauge)
        , histograms :: IORef (HashMap Path Histogram)
        }

metrics :: MonadIO m => m Metrics
metrics = liftIO $ Metrics
    <$> newIORef HM.empty
    <*> newIORef HM.empty
    <*> newIORef HM.empty

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
    maybe initialize return (HM.lookup key hMap)
  where
    initialize = do
        val <- initializer
        atomicModifyIORef' mapRef $ \m -> (HM.insert key val m, val)

-----------------------------------------------------------------------------
-- Counter specifics

newCounter :: Path -> IO Counter
newCounter p = P.register $ P.counter (toInfo p)

counterGet :: MonadIO m => Path -> Metrics -> m Counter
counterGet p m = getOrCreate (counters m) p (newCounter p)

counterAdd :: MonadIO m => Double -> Path -> Metrics -> m ()
counterAdd x p m = liftIO $ do
    c <- counterGet p m
    void $ P.addCounter c x

counterIncr :: MonadIO m => Path -> Metrics -> m ()
counterIncr = counterAdd 1

counterValue :: MonadIO m => Counter -> m Double
counterValue c = P.getCounter c

-----------------------------------------------------------------------------
-- Gauge specifics

newGauge :: Path -> IO Gauge
newGauge p = P.register $ P.gauge (toInfo p )

gaugeGet :: MonadIO m => Path -> Metrics -> m Gauge
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

gaugeValue :: MonadIO m => Gauge -> m Double
gaugeValue g = liftIO $ P.getGauge g

-----------------------------------------------------------------------------
-- Histogram specifics

-- | *DEPRECATED*
-- These are the exact histogram bucket markers which the old *custom* metrics-core
-- library used. Some grafana graphs are still built around these exact number
-- e.g. see gally's POST duration graph:
--   https://staging-ie-grafana.zinfra.io/dashboard/db/galley
--
-- This is annoying and very fragile, prometheus has a better way of handling this, but
-- until we've converted all of the dashboards over to use prometheus rather than collect-d
-- we're stuck with these exact bucket counts.
--
-- Once we use prometheus metrics (e.g. there are no graphs in grafana which depend on metrics
-- prefixed with @collectd@) then you can delete this middleware entirely since the prometheus
-- middleware records request durations already.
deprecatedRequestDurationHistogram :: Path -> HistogramInfo
deprecatedRequestDurationHistogram pth = customHistogram pth requestDurationBuckets
    where
      requestDurationBuckets = [0, 30, 42, 60, 85, 120, 170, 240, 339, 480, 679, 960, 1358]

type Bucket = Double
type Buckets = [Bucket]
data HistogramInfo =
    HistogramInfo
        { hiPath    :: Path
        , hiBuckets :: Buckets
        } deriving (Eq, Show)

type RangeStart = Double
type RangeEnd = Double
type BucketWidth = Double

linearHistogram :: Path -> RangeStart -> RangeEnd -> BucketWidth -> HistogramInfo
linearHistogram pth start end width =
    HistogramInfo
    { hiPath    = pth
    , hiBuckets = buckets
    }
  where
    -- | How many buckets exist between start and end of the given width
    --   We round up because more precision is better than less
    count :: Int
    count   = ceiling $ (end - start) / width
    buckets :: Buckets
    buckets = P.linearBuckets start width count

exponentialHistogram :: Path -> Buckets -> HistogramInfo
exponentialHistogram = HistogramInfo

customHistogram :: Path -> Buckets -> HistogramInfo
customHistogram pth buckets = HistogramInfo{hiPath=pth, hiBuckets=buckets}

newHisto :: HistogramInfo -> IO Histogram
newHisto HistogramInfo {hiPath, hiBuckets} =
    P.register $ P.histogram (toInfo hiPath) hiBuckets

histoGet :: MonadIO m
  => HistogramInfo -- ^ construct using 'makeLinearHistogram or 'makeExponentialHistogram'
  -> Metrics
  -> m Histogram
histoGet hi@HistogramInfo{hiPath} m = getOrCreate (histograms m) hiPath (newHisto hi)

histoValue :: MonadIO m => Histogram -> m (M.Map Bucket Int)
histoValue histo = liftIO $ P.getHistogram histo

-- | Report an individual value to be bucketed in the histogram
histoSubmit :: MonadIO m => Double -> HistogramInfo -> Metrics -> m ()
histoSubmit val hi m = liftIO $ do
    h <- histoGet hi m
    P.observe h val

-- | TODO WRITE DOCS
-- NOTE: If the action throws an exception it will NOT be reported.
-- This is particularly relevant for web handlers which signal their response
-- with an exception.
histoTimeAction :: (P.MonadMonitor m, MonadIO m) => HistogramInfo -> Metrics -> m a -> m a
histoTimeAction hi m act = do
    h <- histoGet hi m
    P.observeDuration h act

-----------------------------------------------------------------------------
-- JSON rendering

class Jsonable a where
    toJson :: a -> IO Value

instance Jsonable Counter where
    toJson c = toJSON <$> counterValue c

instance Jsonable Gauge where
    toJson g = toJSON <$> gaugeValue g

instance Jsonable Histogram where
    -- Note that we round the keys into integers here for back-compatibility because 
    -- some metrics are constructed using the keys of this map (as integers) and having Double
    -- keys would break the dashboards. This will no longer matter once all dashboards using
    -- collectd have been migrated to use prometheus-backed metrics instead.
    toJson h = toJSON . M.mapKeys (round @Double @Integer) <$> histoValue h

render :: MonadIO m => Metrics -> m Value
render m = liftIO $ do
    c <- snapshot =<< readIORef (counters m)
    g <- snapshot =<< readIORef (gauges m)
    b <- snapshot =<< readIORef (histograms m)
    gc <- GC.toJson
    let result = c `union` g `union` b
    return $ maybe result (union result) gc
  where
    snapshot :: Jsonable a => HashMap Path a -> IO Value
    snapshot = fmap object . mapM (\(k, v) -> (_path k .=) <$> toJson v) . HM.toList

    union :: Value -> Value -> Value
    union (Object a) (Object b) = Object $ a `merge` b
    union (Array  a) (Array  b) = Array  $ a <> b
    union Null       b          = b
    union a          _          = a

merge :: Object -> Object -> Object
merge a = expand (expand mempty a)
  where
    expand :: Object -> Object -> Object
    expand = HM.foldrWithKey (\k v obj -> insert obj (T.splitOn "." k) v)

    insert :: Object -> [Text] -> Value -> Object
    insert obj [t]    v = HM.insert t v obj
    insert obj (t:tt) v = HM.insert t (Object $ insert (subtree t obj) tt v) obj
    insert obj []     _ = obj

    subtree :: Text -> Object -> Object
    subtree t o = case HM.lookup t o of
        Just (Object x) -> x
        _               -> mempty
