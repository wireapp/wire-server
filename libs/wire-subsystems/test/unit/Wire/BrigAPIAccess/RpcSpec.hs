module Wire.BrigAPIAccess.RpcSpec where

import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.BrigAPIAccess.Rpc

spec :: Spec
spec =
  describe "BrigAPIAccess.Rpc" $ do
    describe "chunkify" $ do
      prop "is inverse of chunk concatenation" $ mapSize (* 314) $ \(xs :: [Int]) ->
        chunkify pure xs === Just xs
