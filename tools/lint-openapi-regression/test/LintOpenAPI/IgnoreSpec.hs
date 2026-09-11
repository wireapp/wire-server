module LintOpenAPI.IgnoreSpec (spec) where

import Data.Map.Strict qualified as Map
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
      let imap = Map.singleton "v5" (Set.singleton "my-route")
          ctx = mkCtx (Just 5) (Just "my-route") GET "/path"
      isIgnored imap ctx `shouldBe` True

    it "ignores a violation by rendered route" $ do
      let imap = Map.singleton "v5" (Set.singleton "GET /path/{_}")
          ctx = mkCtx (Just 5) Nothing GET "/path/{_}"
      isIgnored imap ctx `shouldBe` True

    it "does not ignore if version mismatch" $ do
      let imap = Map.singleton "v6" (Set.singleton "my-route")
          ctx = mkCtx (Just 5) (Just "my-route") GET "/path"
      isIgnored imap ctx `shouldBe` False

    it "does not ignore if route mismatch" $ do
      let imap = Map.singleton "v5" (Set.singleton "other-route")
          ctx = mkCtx (Just 5) (Just "my-route") GET "/path"
      isIgnored imap ctx `shouldBe` False

  describe "updateIgnoreMap" $ do
    it "adds new violations to the ignore map" $ do
      let imap = Map.singleton "v5" (Set.singleton "old-route")
          ctx1 = mkCtx (Just 5) (Just "new-route") GET "/path1"
          ctx2 = mkCtx (Just 6) Nothing POST "/path2"
          newMap = updateIgnoreMap imap [ctx1, ctx2]

      Map.lookup "v5" newMap `shouldBe` Just (Set.fromList ["old-route", "new-route"])
      Map.lookup "v6" newMap `shouldBe` Just (Set.singleton "POST /path2")

-- | Helper to create a ViolationContext for testing
mkCtx :: Maybe Int -> Maybe Text -> HttpMethod -> Text -> ViolationContext
mkCtx ver opId method routeStr =
  let rk =
        RouteKey
          { method = method,
            route = NormalizedRoute [Literal (Text.drop 1 routeStr)] -- simplistic mock
          }
   in ViolationContext
        { baselineVersion = ver,
          routeKey = rk,
          routeId = opId,
          violation = RouteRemoved
        }
