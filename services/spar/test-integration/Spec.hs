module Main where

import qualified Test.Spar.APISpec
import qualified Test.Spar.DataSpec
import Test.Hspec

main :: IO ()
main = hspec =<< spec

spec :: IO Spec
spec = do
  let specData = describe "Test.Spar.Data" Test.Spar.DataSpec.spec
  specAPI <- describe "Test.Spar.API" <$> Test.Spar.APISpec.mkspec
  pure $ specData >> specAPI
