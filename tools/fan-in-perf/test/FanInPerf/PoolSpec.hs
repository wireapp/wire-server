module FanInPerf.PoolSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import FanInPerf.Pool
import Hasql.Pool.Extended (HasqlPoolMetrics (..))
import Imports
import Prometheus qualified as P
import Test.Hspec

spec :: Spec
spec = describe "poolMetrics" $ do
  -- the registry is global: register the families once
  fams <- runIO mkPoolFamilies
  it "labels the roles writer and reader" $ do
    mapM_ (poolMetrics fams) [WriterPool, ReaderPool]
    out <- LBS8.unpack <$> P.exportMetricsAsText
    out `shouldContainLine` "fanin_perf_pool_in_use{role=\"writer\"} 0.0"
    out `shouldContainLine` "fanin_perf_pool_in_use{role=\"reader\"} 0.0"
  it "gives each role its own handles" $ do
    w <- poolMetrics fams WriterPool
    r <- poolMetrics fams ReaderPool
    _ <- P.addCounter w.sessionCounter 3
    wCount <- P.getCounter w.sessionCounter
    rCount <- P.getCounter r.sessionCounter
    (wCount, rCount) `shouldBe` (3, 0)
  where
    shouldContainLine out l = (l `elem` lines out) `shouldBe` True
