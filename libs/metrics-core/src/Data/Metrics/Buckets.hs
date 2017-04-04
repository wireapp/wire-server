module Data.Metrics.Buckets
    ( Buckets
    , start
    , create
    , index
    , incr
    , snapshot
    , toJson
    ) where

import Data.Aeson
import Data.Atomics.Counter (AtomicCounter)
import Data.HashMap.Strict (HashMap)
import Data.List (nub)
import Data.Text (pack)
import Data.Vector (Vector, (!))

import qualified Data.Atomics.Counter as Atomic
import qualified Data.HashMap.Strict  as Map
import qualified Data.Vector          as Vec

data Buckets = Buckets
    { start   :: Int
    , labels  :: [Int]
    , zero    :: AtomicCounter
    , buckets :: Vector AtomicCounter
    }

create :: Int -> Int -> IO Buckets
create b n = do
    let ll = take n . nub $ zipWith ($) (repeat (gen . fromIntegral)) [0::Int ..]
    Buckets b (reverse ll) <$> Atomic.newCounter 0 <*> Vec.replicateM n (Atomic.newCounter 0)
  where
    gen :: Double -> Int
    gen i = round $ fromIntegral b * (2.0 ** (i / 2.0))

index :: Buckets -> Word -> Int
index b i = truncate $
    logBase (2.0 :: Double) (fromIntegral i / fromIntegral (start b)) * 2.0

incr :: Buckets -> Word -> IO ()
incr b n = case index b n of
    i | i < 0 ->
            Atomic.incrCounter_ 1 (zero b)
      | i >= Vec.length (buckets b) ->
            Atomic.incrCounter_ 1 (Vec.last (buckets b))
      | otherwise ->
            Atomic.incrCounter_ 1 (buckets b ! i)

snapshot :: Buckets -> IO (HashMap Int Word)
snapshot b = do
    z <- fromIntegral <$> Atomic.readCounter (zero b)
    n <- Vec.foldM' (\acc r -> (:acc) . fromIntegral <$> Atomic.readCounter r) [] (buckets b)
    return $ Map.fromList ((0, z) : zip (labels b) n)

toJson :: Buckets -> IO Value
toJson b = object
    . map (\(k, v) -> pack (show k) .= toJSON v)
    . Map.toList
   <$> snapshot b
