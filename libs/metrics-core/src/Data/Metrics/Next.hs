{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Metrics.Next
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

import qualified Data.Atomics.Counter as Atomic
import qualified Data.Text            as T
import qualified Data.HashMap.Strict  as Map
import qualified Data.Metrics.Buckets as Buckets

newtype Path = Path { _path :: Text } deriving (Eq, Hashable, Semigroup, Monoid)

toPrometheusMetric :: Text -> Text
toPrometheusMetric = undefined

path :: Text -> Path
path = Path . toPrometheusMetric

newtype Counter = Counter AtomicCounter
newtype Gauge   = Gauge   AtomicCounter
newtype Label   = Label   (IORef Text)

data Metrics = Metrics
    { counters :: IORef (HashMap Path Counter)
    , gauges   :: IORef (HashMap Path Gauge)
    , labels   :: IORef (HashMap Path Label)
    , buckets  :: IORef (HashMap Path Buckets)
    }

metrics :: MonadIO m => m Metrics
metrics = liftIO $ Metrics
    <$> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty

-----------------------------------------------------------------------------
-- Counter specifics

counterGet :: MonadIO m => Path -> Metrics -> m Counter
counterGet t m = liftIO $ do
    cs <- readIORef (counters m)
    maybe make return (Map.lookup t cs)
  where
    make = do
        c <- Counter <$> Atomic.newCounter 0
        atomicModifyIORef' (counters m) $ \cs -> (Map.insert t c cs, c)

counterGet' :: MonadIO m => Path -> Metrics -> m Counter
counterGet' t m = liftIO $ do
    c <- Counter <$> Atomic.newCounter 0
    atomicModifyIORef' (counters m) $ \cs ->
        case Map.lookup t cs of
            Nothing -> (Map.insert t c cs, c)
            Just c' -> (cs , c')

counterAdd :: MonadIO m => Word -> Path -> Metrics -> m ()
counterAdd x p m = liftIO $ do
    Counter c <- counterGet p m
    Atomic.incrCounter_ (fromIntegral x) c

counterIncr :: MonadIO m => Path -> Metrics -> m ()
counterIncr = counterAdd 1

counterValue :: MonadIO m => Counter -> m Word
counterValue (Counter c) = liftIO $ fromIntegral <$> Atomic.readCounter c

-----------------------------------------------------------------------------
-- Gauge specifics

gaugeGet :: MonadIO m => Path -> Metrics -> m Gauge
gaugeGet t m = liftIO $ do
    gs <- readIORef (gauges m)
    maybe make return (Map.lookup t gs)
  where
    make = do
        g <- Gauge <$> Atomic.newCounter 0
        atomicModifyIORef' (gauges m) $ \gs -> (Map.insert t g gs, g)

gaugeGet' :: MonadIO m => Path -> Metrics -> m Gauge
gaugeGet' t m = liftIO $ do
    g <- Gauge <$> Atomic.newCounter 0
    atomicModifyIORef' (gauges m) $ \gs ->
        case Map.lookup t gs of
            Nothing -> (Map.insert t g gs, g)
            Just g' -> (gs , g')

gaugeSet :: MonadIO m => Int -> Path -> Metrics -> m ()
gaugeSet x p m = liftIO $ do
    Gauge g <- gaugeGet p m
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

-----------------------------------------------------------------------------
-- Label specifics

labelGet :: MonadIO m => Path -> Metrics -> m Label
labelGet t m = liftIO $ do
    ls <- readIORef (labels m)
    maybe make return (Map.lookup t ls)
  where
    make = do
        l <- Label <$> newIORef T.empty
        atomicModifyIORef' (labels m) $ \ls -> (Map.insert t l ls, l)

labelGet' :: MonadIO m => Path -> Metrics -> m Label
labelGet' t m = liftIO $ do
    l <- Label <$> newIORef T.empty
    atomicModifyIORef' (labels m) $ \ls ->
        case Map.lookup t ls of
            Nothing -> (Map.insert t l ls, l)
            Just l' -> (ls , l')

labelSet :: MonadIO m => Text -> Path -> Metrics -> m ()
labelSet x p m = liftIO $ do
    Label l <- labelGet p m
    atomicModifyIORef' l $ const (x, ())

labelValue :: MonadIO m => Label -> m Text
labelValue (Label l) = liftIO $ readIORef l

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

bucketsGet' :: MonadIO m => Int -> Int -> Path -> Metrics -> m Buckets
bucketsGet' k n t m = liftIO $ do
    b <- Buckets.create k n
    atomicModifyIORef' (buckets m) $ \bs ->
        case Map.lookup t bs of
            Nothing -> (Map.insert t b bs, b)
            Just b' -> (bs , b')

bucketsIncr :: MonadIO m => Int -> Int -> Word -> Path -> Metrics -> m ()
bucketsIncr k n x p m = liftIO $ do
    b <- bucketsGet k n p m
    Buckets.incr b x

-----------------------------------------------------------------------------
-- JSON rendering

class Jsonable a where
    toJson :: a -> IO Value

instance Jsonable Counter where
    toJson c = toJSON <$> counterValue c

instance Jsonable Gauge where
    toJson g = toJSON <$> gaugeValue g

instance Jsonable Label where
    toJson l = toJSON <$> labelValue l

instance Jsonable Buckets where
    toJson = Buckets.toJson

render :: MonadIO m => Metrics -> m Value
render m = liftIO $ do
    c <- snapshot =<< readIORef (counters m)
    g <- snapshot =<< readIORef (gauges m)
    l <- snapshot =<< readIORef (labels m)
    b <- snapshot =<< readIORef (buckets m)
    h <- GC.toJson
    let result = c `union` g `union` l `union` b
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
