module Test.Spar.BogusSpec where

import Imports
import Test.Hspec

spec :: Spec
spec = do
  it "should encourage the CI by having a change" $
    True `shouldBe` True
