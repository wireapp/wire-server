module LintOpenAPI.BaselineSpec (spec) where

import Data.Map.Strict qualified as Map
import Imports
import LintOpenAPI.Baseline
import LintOpenAPI.Types
import Test.Hspec

spec :: Spec
spec = describe "LintOpenAPI.Baseline" $ do
  describe "selectBaselineFile" $ do
    let files = ["dir/swagger-v15.json", "dir/swagger-v16.json", "dir/swagger-v18.json"]

    it "picks the baseline matching the requested version" $ do
      selectBaselineFile 16 files `shouldBe` Right "dir/swagger-v16.json"

    it "fails for a version with no baseline file" $ do
      selectBaselineFile 17 files
        `shouldBe` Left
          "No saved baseline for version v17: swagger-v17.json not found (only OpenAPI 3 baselines, v5+, are supported)"

    it "fails for an OpenAPI 2 filename (v3) even if present" $ do
      selectBaselineFile 3 ("dir/swagger-v3.json" : files)
        `shouldBe` Left
          "No saved baseline for version v3: swagger-v3.json not found (only OpenAPI 3 baselines, v5+, are supported)"

    it "fails when there are no baselines at all" $ do
      selectBaselineFile 5 [] `shouldSatisfy` isLeft

  describe "expectVersion" $ do
    let specWith v = OpenAPISpec {version = v, routes = Map.empty}

    it "succeeds on a matching version" $ do
      expectVersion 16 (specWith (Just 16)) `shouldBe` Right ()

    it "fails on a version mismatch" $ do
      expectVersion 15 (specWith (Just 16)) `shouldBe` Left "expected v15, found v16"

    it "fails when the spec has no version" $ do
      expectVersion 16 (specWith Nothing) `shouldBe` Left "spec has no version (servers[0].url)"
