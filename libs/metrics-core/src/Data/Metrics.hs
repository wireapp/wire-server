{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Metrics
    ( Path
    , Metrics
    , Counter
    , Gauge
    , Label
    , Buckets

    , path
    , metrics

    , counterGet
    , counterGet'
    , counterAdd
    , counterIncr
    , counterValue

    , gaugeGet
    , gaugeGet'
    , gaugeAdd
    , gaugeSub
    , gaugeIncr
    , gaugeDecr
    , gaugeSet
    , gaugeValue

    , labelGet
    , labelGet'
    , labelSet
    , labelValue

    , bucketsGet
    , bucketsGet'
    , bucketsIncr

    , render
    ) where

import Imports hiding (lookup, union)
import Data.Aeson
import Data.Atomics.Counter (AtomicCounter)
import Data.Hashable
import Data.Metrics.Buckets (Buckets)
import Data.Metrics.Types

import qualified Data.Atomics.Counter  as Atomic
import qualified Data.Text             as T
import qualified Data.HashMap.Strict   as Map
import qualified Data.Metrics.Buckets  as Buckets
import qualified Data.Metrics.GC       as GC
import qualified Data.Metrics.CollectD as CD

import qualified Prometheus as P

class MonadMetrics m where
    getMetrics :: m Metrics

data Counter = Counter P.Counter CD.Counter
data Gauge   = Gauge   P.Gauge CD.Gauge

data Metrics = Metrics
    { counters :: IORef (HashMap Path Counter)
    , gauges   :: IORef (HashMap Path Gauge)
    -- , labels   :: IORef (HashMap Path Label)
    -- , buckets  :: IORef (HashMap Path Buckets)
    }

-- | Converts a CollectD style 'path' to a Metric name usable by prometheus
--
-- Currently just replaces all "/" with "_" and lowercases the result
toPInfo :: Path -> P.Info
toPInfo (Path p) = P.Info (p & T.replace "/" "_"
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
-- Counter specifics

newCounter :: MonadIO m => Path -> m Counter
newCounter p = liftIO $ liftA2 Counter (P.register pCounter) (CD.Counter <$> (Atomic.newCounter 0))
    where
        pCounter = P.counter (toPInfo p)

counterGet :: MonadIO m => Path -> Metrics -> m Counter
counterGet t m = liftIO $ do
    getOrCreate (counters m) t (newCounter t)

counterAdd :: MonadIO m => Word -> Path -> Metrics -> m ()
counterAdd x p m = liftIO $ do
    Counter pCounter (CD.Counter cdCounter) <- counterGet p m
    Atomic.incrCounter_ (fromIntegral x) cdCounter

counterIncr :: MonadIO m => Path -> Metrics -> m ()
counterIncr = counterAdd 1

-----------------------------------------------------------------------------
-- Gauge specifics

newGauge :: MonadIO m => Path -> m Gauge
newGauge p = liftIO $ liftA2 Gauge (P.register pGauge) (CD.Gauge <$> (Atomic.newCounter 0))
    where
        pGauge = P.gauge (toPInfo p)

gaugeGet :: MonadIO m => Path -> Metrics -> m Gauge
gaugeGet t m = liftIO $ do
    getOrCreate (gauges m) t (newGauge t)

gaugeSet :: MonadIO m => Int -> Path -> Metrics -> m ()
gaugeSet x p m = liftIO $ do
    Gauge pGauge cdGauge <- gaugeGet p m
    -- To play it safe, we want a full memory barrier, which 'writeCounter'
    -- does not provide, so we use a CAS loop instead. This is not worse
    -- than e.g. a CAS loop inherent to a comparable 'atomicModifyIORef'.
    casLoop g =<< Atomic.readCounterForCAS g
  where
    casLoop g v = do
        (ok, v') <- Atomic.casCounter g v x
        unless ok (casLoop g v')

gaugeAdd :: MonadIO m => Int -> Path -> Metrics -> m ()
gaugeAdd x p m = liftIO $ do
    Gauge g <- gaugeGet p m
    Atomic.incrCounter_ x g

gaugeIncr :: MonadIO m => Path -> Metrics -> m ()
gaugeIncr = gaugeAdd 1

gaugeDecr :: MonadIO m => Path -> Metrics -> m ()
gaugeDecr = gaugeAdd (-1)

gaugeSub :: MonadIO m => Int -> Path -> Metrics -> m ()
gaugeSub x = gaugeAdd (-x)

gaugeValue :: MonadIO m => Gauge -> m Int
gaugeValue (Gauge g) = liftIO $ Atomic.readCounter g
