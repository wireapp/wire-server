module FanInPerf.SettleSpec (spec) where

import FanInPerf.Settle
import Imports
import Test.Hspec

-- | Reads the values in turn, the last one forever.
sequenceOf :: [Int] -> IO (IO Int)
sequenceOf vs = do
  ref <- newIORef vs
  pure $ atomicModifyIORef' ref $ \case
    [v] -> ([v], v)
    (v : rest) -> (rest, v)
    [] -> error "no values"

spec :: Spec
spec = describe "waitUntilStable" $ do
  it "returns the value once it stayed the same for the given time" $ do
    next <- sequenceOf [10, 9, 8, 8]
    waitUntilStable 0.1 5 0.02 next `shouldReturn` Just 8
  it "starts counting again when the value changes" $ do
    -- 5 stays for 0.05 s only, then it changes
    next <- sequenceOf [5, 5, 5, 6, 7]
    waitUntilStable 0.2 5 0.02 next `shouldReturn` Just 7
  it "gives up after the timeout" $ do
    ref <- newIORef (0 :: Int)
    let changing = atomicModifyIORef' ref (\n -> (n + 1, n))
    waitUntilStable 0.5 0.1 0.02 changing `shouldReturn` Nothing
  it "returns at once with no waiting time required" $ do
    waitUntilStable 0 5 0.02 (pure (1 :: Int)) `shouldReturn` Just 1
