module Test.Spar.OptionsSpec where

import Test.Hspec
import Spar.Options
import Util.Options


spec :: Spec
spec = do
  it "parse returns result that contains no impure exceptions" $ do
    result <- getOptions "desc" cliOptsParser "spar.integration.yaml"
    length (show result) `shouldNotBe` 0
