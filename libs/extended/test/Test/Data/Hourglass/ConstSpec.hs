module Test.Data.Hourglass.ConstSpec where

import Data.Hourglass
import Data.Hourglass.Const
import Imports
import Test.Hspec

spec :: Spec
spec =
  describe "Data.Hourglass.Const" $ do
    describe "midnight" $ do
      it "should represent midnight (00:00:00.000)" $ do
        let TimeOfDay (Hours h) (Minutes m) (Seconds s) ns = midnight
        h `shouldBe` 0
        m `shouldBe` 0
        s `shouldBe` 0
        ns `shouldBe` 0
