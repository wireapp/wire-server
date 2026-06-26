module LintOpenAPI.ParseSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Imports
import LintOpenAPI.Parse
import LintOpenAPI.Types
import Test.Hspec

spec :: Spec
spec = describe "LintOpenAPI.Parse" $ do
  describe "parseOpenAPIValue" $ do
    it "parses a minimal OpenAPI spec" $ do
      let val =
            object
              [ "openapi" .= ("3.0.0" :: Text),
                "paths" .= object [],
                "info" .= object ["title" .= ("Test" :: Text), "version" .= ("" :: Text)]
              ]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          spec'.version `shouldBe` Nothing
          Map.size spec'.routes `shouldBe` 0

    it "extracts version from servers" $ do
      let val =
            object
              [ "openapi" .= ("3.0.0" :: Text),
                "paths" .= object [],
                "servers" .= [object ["url" .= ("/v5" :: Text)]]
              ]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> spec'.version `shouldBe` Just 5

    it "parses a route with query parameters" $ do
      let val = mkSpec [("/users", "get", mkOperation [mkQueryParam "size" False, mkQueryParam "start" True] Nothing [])]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          length spec'.routes `shouldBe` 1
          ri <- headRoute spec'.routes
          ri.queryParams `shouldBe` Set.fromList ["size", "start"]
          ri.requiredQueryParams `shouldBe` Set.singleton "start"

    it "parses a route with request body" $ do
      let bodySchema =
            object
              [ "type" .= ("object" :: Text),
                "required" .= (["name", "email"] :: [Text]),
                "properties"
                  .= object
                    [ "name" .= object ["type" .= ("string" :: Text)],
                      "email" .= object ["type" .= ("string" :: Text)],
                      "nickname" .= object ["type" .= ("string" :: Text)]
                    ]
              ]
          val = mkSpec [("/users", "post", mkOperation [] (Just bodySchema) [])]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          ri <- headRoute spec'.routes
          case ri.requestBody of
            Nothing -> expectationFailure "Expected request body"
            Just schema -> do
              schema.requiredFields `shouldBe` Set.fromList ["name", "email"]
              Map.keys schema.properties `shouldSatisfy` ("nickname" `elem`)

    it "parses a route with response body" $ do
      let respSchema =
            object
              [ "type" .= ("object" :: Text),
                "required" .= (["id", "name"] :: [Text]),
                "properties"
                  .= object
                    [ "id" .= object ["type" .= ("string" :: Text)],
                      "name" .= object ["type" .= ("string" :: Text)],
                      "phone" .= object ["type" .= ("string" :: Text)]
                    ]
              ]
          val = mkSpec [("/self", "get", mkOperation [] Nothing [("200", respSchema)])]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          ri <- headRoute spec'.routes
          case Map.lookup "200" ri.responses of
            Nothing -> expectationFailure "Expected 200 response"
            Just schema -> do
              schema.requiredFields `shouldBe` Set.fromList ["id", "name"]
              Map.size schema.properties `shouldBe` 3

    it "resolves $ref in request body" $ do
      let val =
            object
              [ "openapi" .= ("3.0.0" :: Text),
                "paths"
                  .= object
                    [ "/users"
                        .= object
                          [ "post"
                              .= object
                                [ "requestBody"
                                    .= object
                                      [ "content"
                                          .= object
                                            [ "application/json"
                                                .= object
                                                  ["schema" .= object ["$ref" .= ("#/components/schemas/User" :: Text)]]
                                            ]
                                      ]
                                ]
                          ]
                    ],
                "components"
                  .= object
                    [ "schemas"
                        .= object
                          [ "User"
                              .= object
                                [ "type" .= ("object" :: Text),
                                  "required" .= (["name"] :: [Text]),
                                  "properties"
                                    .= object
                                      [ "name" .= object ["type" .= ("string" :: Text)]
                                      ]
                                ]
                          ]
                    ]
              ]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          ri <- headRoute spec'.routes
          case ri.requestBody of
            Nothing -> expectationFailure "Expected request body"
            Just schema -> do
              schema.requiredFields `shouldBe` Set.singleton "name"
              Map.keys schema.properties `shouldBe` ["name"]

    it "resolves allOf schemas" $ do
      let val =
            object
              [ "openapi" .= ("3.0.0" :: Text),
                "paths"
                  .= object
                    [ "/items"
                        .= object
                          [ "post"
                              .= object
                                [ "requestBody"
                                    .= object
                                      [ "content"
                                          .= object
                                            [ "application/json"
                                                .= object
                                                  [ "schema"
                                                      .= object
                                                        [ "allOf"
                                                            .= [ object ["$ref" .= ("#/components/schemas/Base" :: Text)],
                                                                 object
                                                                   [ "type" .= ("object" :: Text),
                                                                     "required" .= (["extra"] :: [Text]),
                                                                     "properties" .= object ["extra" .= object ["type" .= ("string" :: Text)]]
                                                                   ]
                                                               ]
                                                        ]
                                                  ]
                                            ]
                                      ]
                                ]
                          ]
                    ],
                "components"
                  .= object
                    [ "schemas"
                        .= object
                          [ "Base"
                              .= object
                                [ "type" .= ("object" :: Text),
                                  "required" .= (["id"] :: [Text]),
                                  "properties" .= object ["id" .= object ["type" .= ("string" :: Text)]]
                                ]
                          ]
                    ]
              ]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          ri <- headRoute spec'.routes
          case ri.requestBody of
            Nothing -> expectationFailure "Expected request body"
            Just schema -> do
              schema.requiredFields `shouldBe` Set.fromList ["id", "extra"]
              Map.size schema.properties `shouldBe` 2

    it "handles enum values" $ do
      let bodySchema =
            object
              [ "type" .= ("string" :: Text),
                "enum" .= (["active", "inactive", "suspended"] :: [Text])
              ]
          val = mkSpec [("/status", "put", mkOperation [] (Just bodySchema) [])]
      case parseOpenAPIValue val of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right spec' -> do
          ri <- headRoute spec'.routes
          case ri.requestBody of
            Nothing -> expectationFailure "Expected request body"
            Just schema ->
              schema.enumValues `shouldBe` Just (Set.fromList ["active", "inactive", "suspended"])

    it "rejects non-object top-level value" $ do
      parseOpenAPIValue (String "not an object") `shouldSatisfy` isLeft

-- | Helper to build a minimal OpenAPI spec value.
mkSpec :: [(Text, Text, Value)] -> Value
mkSpec routes =
  object
    [ "openapi" .= ("3.0.0" :: Text),
      "paths" .= object (map mkPathEntry routes)
    ]
  where
    mkPathEntry (path, method, op) =
      Key.fromText path .= object [Key.fromText method .= op]

-- | Helper to build an operation object.
mkOperation :: [Value] -> Maybe Value -> [(Text, Value)] -> Value
mkOperation params mBody resps =
  object
    $ ["parameters" .= params]
    <> maybe [] (\b -> ["requestBody" .= mkRequestBody b]) mBody
    <> ["responses" .= object (map mkResponse resps)]

-- | Helper to build a query parameter.
mkQueryParam :: Text -> Bool -> Value
mkQueryParam name required =
  object
    [ "name" .= name,
      "in" .= ("query" :: Text),
      "required" .= required
    ]

-- | Helper to wrap a schema in a request body.
mkRequestBody :: Value -> Value
mkRequestBody schema =
  object
    [ "content"
        .= object
          [ "application/json"
              .= object ["schema" .= schema]
          ]
    ]

-- | Helper to wrap a schema in a response.
mkResponse :: (Text, Value) -> (Key.Key, Value)
mkResponse (code, schema) =
  Key.fromText code
    .= object
      [ "content"
          .= object
            [ "application/json"
                .= object ["schema" .= schema]
            ]
      ]

-- | Helper to safely get the first route info.
headRoute :: Map.Map RouteKey RouteInfo -> IO RouteInfo
headRoute m = case listToMaybe (Map.elems m) of
  Just ri -> pure ri
  Nothing -> do
    expectationFailure "Expected at least 1 route"
    error "unreachable"
