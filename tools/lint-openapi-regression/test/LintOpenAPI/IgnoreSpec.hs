module LintOpenAPI.IgnoreSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Imports
import LintOpenAPI.Ignore
import LintOpenAPI.Types
import Test.Hspec

spec :: Spec
spec = describe "LintOpenAPI.Ignore" $ do
  describe "isIgnored" $ do
    it "ignores a violation by operationId" $ do
      let iset = Set.singleton "my-route"
          ctx = mkCtx (Just "my-route") GET "/path"
      isIgnored iset ctx `shouldBe` True

    it "ignores a violation by rendered route" $ do
      let iset = Set.singleton "GET /path/{_}"
          ctx = mkCtx Nothing GET "/path/{_}"
      isIgnored iset ctx `shouldBe` True

    it "does not ignore if route mismatch" $ do
      let iset = Set.singleton "other-route"
          ctx = mkCtx (Just "my-route") GET "/path"
      isIgnored iset ctx `shouldBe` False

    it "ignores regardless of baseline version" $ do
      let iset = Set.singleton "my-route"
          ctx = (mkCtx (Just "my-route") GET "/path") {baselineVersion = Just 7}
      isIgnored iset ctx `shouldBe` True

  describe "collectIgnoreSet" $ do
    it "collects identifiers from all contexts" $ do
      let ctxs = [mkCtx (Just "op-a") GET "/a", mkCtx Nothing POST "/b"]
      collectIgnoreSet ctxs `shouldBe` Set.fromList ["op-a", "POST /b"]

  describe "readIgnoreFile/writeIgnoreFile" $ do
    it "roundtrips a set of route identifiers"
      $ withTempDir
      $ \dir -> do
        let path = dir <> "/v15.json"
            routes = Set.fromList ["getUser", "POST /teams", "GET /users/{_}"]
        writeIgnoreFile path routes
        result <- readIgnoreFile path
        result `shouldBe` Right routes

    it "fails on a missing file"
      $ withTempDir
      $ \dir -> do
        result <- readIgnoreFile (dir <> "/missing.json")
        result `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False

    it "fails on a non-array JSON document"
      $ withTempDir
      $ \dir -> do
        let path = dir <> "/bad.json"
        writeFile path "{\"v5\": [\"x\"]}"
        result <- readIgnoreFile path
        result `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False

-- | Helper to create a ViolationContext for testing
mkCtx :: Maybe Text -> HttpMethod -> Text -> ViolationContext
mkCtx opId method routeStr =
  let rk =
        RouteKey
          { method = method,
            route = NormalizedRoute [Literal (Text.drop 1 routeStr)] -- simplistic mock
          }
   in ViolationContext
        { baselineVersion = Nothing,
          routeKey = rk,
          routeId = opId,
          violation = RouteRemoved
        }

-- | Small helper: run in the system temporary directory.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir k = getTemporaryDirectory >>= k
