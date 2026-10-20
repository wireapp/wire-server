module LintOpenAPI.NormalizeSpec (spec) where

import Imports
import LintOpenAPI.Normalize
import LintOpenAPI.Types
import Test.Hspec

spec :: Spec
spec = describe "LintOpenAPI.Normalize" $ do
  describe "normalizePath" $ do
    it "normalizes a simple path" $ do
      normalizePath "/users"
        `shouldBe` NormalizedRoute [Literal "users"]

    it "normalizes a path with placeholders" $ do
      normalizePath "/users/{userId}/clients/{clientId}"
        `shouldBe` NormalizedRoute [Literal "users", Placeholder, Literal "clients", Placeholder]

    it "normalizes root path" $ do
      normalizePath "/"
        `shouldBe` NormalizedRoute []

    it "normalizes path without leading slash" $ do
      normalizePath "users/{id}"
        `shouldBe` NormalizedRoute [Literal "users", Placeholder]

    it "normalizes path with multiple consecutive slashes" $ do
      -- splitting on "/" with empty segments filtered out
      normalizePath "/users//clients"
        `shouldBe` NormalizedRoute [Literal "users", Literal "clients"]

    it "treats different placeholder names as equal" $ do
      normalizePath "/users/{userId}"
        `shouldBe` normalizePath "/users/{uid}"

    it "treats different literal segments as different" $ do
      normalizePath "/users"
        `shouldNotBe` normalizePath "/teams"

  describe "routeKeyFromPath" $ do
    it "creates a route key for GET" $ do
      let result = routeKeyFromPath "/users/{id}" "get"
      result `shouldBe` Just RouteKey {method = GET, route = NormalizedRoute [Literal "users", Placeholder]}

    it "creates a route key for POST (case insensitive)" $ do
      let result = routeKeyFromPath "/teams" "POST"
      result `shouldBe` Just RouteKey {method = POST, route = NormalizedRoute [Literal "teams"]}

    it "returns Nothing for unknown methods" $ do
      routeKeyFromPath "/foo" "UNKNOWN" `shouldBe` Nothing

    it "handles all HTTP methods" $ do
      routeKeyFromPath "/x" "get" `shouldSatisfy` isJust
      routeKeyFromPath "/x" "post" `shouldSatisfy` isJust
      routeKeyFromPath "/x" "put" `shouldSatisfy` isJust
      routeKeyFromPath "/x" "patch" `shouldSatisfy` isJust
      routeKeyFromPath "/x" "delete" `shouldSatisfy` isJust
      routeKeyFromPath "/x" "head" `shouldSatisfy` isJust
      routeKeyFromPath "/x" "options" `shouldSatisfy` isJust
