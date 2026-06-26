module LintOpenAPI.CompareSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Imports
import LintOpenAPI.Compare
import LintOpenAPI.Types
import Test.Hspec

spec :: Spec
spec = describe "LintOpenAPI.Compare" $ do
  describe "compareSpecs" $ do
    it "detects route removal" $ do
      let baseline = mkOpenAPISpec (Just 5) [(getUsers, emptyRouteInfo)]
          candidate = mkOpenAPISpec Nothing []
          violations = compareSpecs baseline candidate
      length violations `shouldBe` 1
      case listToMaybe violations of
        Just ctx -> ctx.violation `shouldBe` RouteRemoved
        Nothing -> expectationFailure "Expected 1 violation"

    it "passes when route is preserved" $ do
      let baseline = mkOpenAPISpec (Just 5) [(getUsers, emptyRouteInfo)]
          candidate = mkOpenAPISpec Nothing [(getUsers, emptyRouteInfo)]
          violations = compareSpecs baseline candidate
      violations `shouldBe` []

    it "ignores new routes in candidate" $ do
      let baseline = mkOpenAPISpec (Just 5) [(getUsers, emptyRouteInfo)]
          candidate = mkOpenAPISpec Nothing [(getUsers, emptyRouteInfo), (postTeams, emptyRouteInfo)]
          violations = compareSpecs baseline candidate
      violations `shouldBe` []

  describe "compareRouteInfo" $ do
    it "detects query param removal" $ do
      let baseline = emptyRouteInfo {queryParams = Set.fromList ["size", "start"]}
          candidate = emptyRouteInfo {queryParams = Set.singleton "start"}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [QueryParamRemoved "size"]

    it "detects new required query param" $ do
      let baseline = emptyRouteInfo {queryParams = Set.singleton "start"}
          candidate = emptyRouteInfo {queryParams = Set.fromList ["start", "filter"], requiredQueryParams = Set.singleton "filter"}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [RequiredQueryParamAdded "filter"]

    it "allows new optional query param" $ do
      let baseline = emptyRouteInfo {queryParams = Set.singleton "start"}
          candidate = emptyRouteInfo {queryParams = Set.fromList ["start", "sort"]}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` []

    it "detects new required body field" $ do
      let baseBody = emptySchema {requiredFields = Set.singleton "name"}
          candBody = emptySchema {requiredFields = Set.fromList ["name", "team_id"]}
          baseline = emptyRouteInfo {requestBody = Just baseBody}
          candidate = emptyRouteInfo {requestBody = Just candBody}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [RequiredBodyFieldAdded "team_id"]

    it "allows new optional body field" $ do
      let baseBody =
            emptySchema
              { requiredFields = Set.singleton "name",
                properties = Map.singleton "name" emptySchema
              }
          candBody =
            emptySchema
              { requiredFields = Set.singleton "name",
                properties = Map.fromList [("name", emptySchema), ("nickname", emptySchema)]
              }
          baseline = emptyRouteInfo {requestBody = Just baseBody}
          candidate = emptyRouteInfo {requestBody = Just candBody}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` []

    it "detects removed required response field" $ do
      let baseResp =
            emptySchema
              { requiredFields = Set.fromList ["id", "phone"],
                properties = Map.fromList [("id", emptySchema), ("phone", emptySchema)]
              }
          candResp =
            emptySchema
              { requiredFields = Set.singleton "id",
                properties = Map.singleton "id" emptySchema
              }
          baseline = emptyRouteInfo {responses = Map.singleton "200" baseResp}
          candidate = emptyRouteInfo {responses = Map.singleton "200" candResp}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [ResponseFieldRemoved "phone"]

    it "allows removal of optional response field" $ do
      let baseResp =
            emptySchema
              { requiredFields = Set.singleton "id",
                properties = Map.fromList [("id", emptySchema), ("legacy", emptySchema)]
              }
          candResp =
            emptySchema
              { requiredFields = Set.singleton "id",
                properties = Map.singleton "id" emptySchema
              }
          baseline = emptyRouteInfo {responses = Map.singleton "200" baseResp}
          candidate = emptyRouteInfo {responses = Map.singleton "200" candResp}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` []

    it "detects enum value removed in request body" $ do
      let baseBody = emptySchema {enumValues = Just (Set.fromList ["active", "expired"])}
          candBody = emptySchema {enumValues = Just (Set.singleton "active")}
          baseline = emptyRouteInfo {requestBody = Just baseBody}
          candidate = emptyRouteInfo {requestBody = Just candBody}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [EnumValueRemoved "" "expired"]

    it "allows enum value addition in request body" $ do
      let baseBody = emptySchema {enumValues = Just (Set.singleton "active")}
          candBody = emptySchema {enumValues = Just (Set.fromList ["active", "archived"])}
          baseline = emptyRouteInfo {requestBody = Just baseBody}
          candidate = emptyRouteInfo {requestBody = Just candBody}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` []

    it "allows enum value removed from response body" $ do
      let baseResp = emptySchema {enumValues = Just (Set.fromList ["active", "inactive"])}
          candResp = emptySchema {enumValues = Just (Set.singleton "active")}
          baseline = emptyRouteInfo {responses = Map.singleton "200" baseResp}
          candidate = emptyRouteInfo {responses = Map.singleton "200" candResp}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` []

    it "detects enum value added in response body" $ do
      let baseResp = emptySchema {enumValues = Just (Set.singleton "active")}
          candResp = emptySchema {enumValues = Just (Set.fromList ["active", "archived"])}
          baseline = emptyRouteInfo {responses = Map.singleton "200" baseResp}
          candidate = emptyRouteInfo {responses = Map.singleton "200" candResp}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [EnumValueAdded "" "archived"]

    it "handles placeholder name changes (treated as same route)" $ do
      let rk1 =
            RouteKey
              { method = GET,
                route = NormalizedRoute [Literal "users", Placeholder]
              }
          rk2 = rk1 -- Same structure, normalized
          baseline = mkOpenAPISpec (Just 5) [(rk1, emptyRouteInfo)]
          candidate = mkOpenAPISpec Nothing [(rk2, emptyRouteInfo)]
          violations = compareSpecs baseline candidate
      violations `shouldBe` []

    it "handles body appearing where none existed" $ do
      let candBody = emptySchema {requiredFields = Set.singleton "team"}
          baseline = emptyRouteInfo {requestBody = Nothing}
          candidate = emptyRouteInfo {requestBody = Just candBody}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` [RequiredBodyFieldAdded "team"]

    it "handles body disappearing (no violation)" $ do
      let baseBody = emptySchema {requiredFields = Set.singleton "name"}
          baseline = emptyRouteInfo {requestBody = Just baseBody}
          candidate = emptyRouteInfo {requestBody = Nothing}
          violations = compareRouteInfo baseline candidate
      violations `shouldBe` []

-- | Helper route keys for tests.
getUsers :: RouteKey
getUsers =
  RouteKey
    { method = GET,
      route = NormalizedRoute [Literal "users"]
    }

postTeams :: RouteKey
postTeams =
  RouteKey
    { method = POST,
      route = NormalizedRoute [Literal "teams"]
    }

-- | Helper to build an empty route info.
emptyRouteInfo :: RouteInfo
emptyRouteInfo =
  RouteInfo
    { operationId = Nothing,
      queryParams = Set.empty,
      requiredQueryParams = Set.empty,
      requestBody = Nothing,
      responses = Map.empty
    }

-- | Helper to build a spec from route key/info pairs.
mkOpenAPISpec :: Maybe Int -> [(RouteKey, RouteInfo)] -> OpenAPISpec
mkOpenAPISpec ver rs =
  OpenAPISpec
    { version = ver,
      routes = Map.fromList rs
    }
