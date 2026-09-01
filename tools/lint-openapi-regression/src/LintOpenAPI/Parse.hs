module LintOpenAPI.Parse
  ( parseOpenAPIFile,
    parseOpenAPIValue,
  )
where

import Data.Aeson (Value (..), eitherDecodeFileStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Imports
import LintOpenAPI.Normalize
import LintOpenAPI.Types

-- | Parse an OpenAPI JSON file from disk into an 'OpenAPISpec'.
parseOpenAPIFile :: FilePath -> IO (Either String OpenAPISpec)
parseOpenAPIFile fp = do
  result <- eitherDecodeFileStrict' fp
  pure $ result >>= parseOpenAPIValue

-- | Parse an OpenAPI JSON 'Value' into an 'OpenAPISpec'.
parseOpenAPIValue :: Value -> Either String OpenAPISpec
parseOpenAPIValue val = case val of
  Object top -> do
    let schemas = extractSchemas top
        ver = extractVersion top
    rs <- extractRoutes schemas top
    Right
      OpenAPISpec
        { version = ver,
          routes = rs
        }
  _ -> Left "Top-level value must be a JSON object"

-- | Extract the API version from servers[0].url (e.g., "/v5" -> 5).
extractVersion :: KM.KeyMap Value -> Maybe Int
extractVersion top = do
  Array servers <- KM.lookup "servers" top
  Object server <- servers Vector.!? 0
  String url <- KM.lookup "url" server
  vText <- Text.stripPrefix "/v" url
  readMaybe (Text.unpack vText)

-- | Extract the component schemas map for $ref resolution.
extractSchemas :: KM.KeyMap Value -> Map.Map Text Value
extractSchemas top = case KM.lookup "components" top of
  Just (Object comps) -> case KM.lookup "schemas" comps of
    Just (Object schemaMap) ->
      Map.fromList
        [(Key.toText k, v) | (k, v) <- KM.toList schemaMap]
    _ -> Map.empty
  _ -> Map.empty

-- | Extract all routes from the paths object.
extractRoutes ::
  Map.Map Text Value ->
  KM.KeyMap Value ->
  Either String (Map.Map RouteKey RouteInfo)
extractRoutes schemas top = case KM.lookup "paths" top of
  Just (Object pathsObj) -> do
    let pairs =
          [ (rk, ri)
          | (pathKey, pathVal) <- KM.toList pathsObj,
            let path = Key.toText pathKey,
            (rk, ri) <- extractPathMethods schemas path pathVal
          ]
    Right (Map.fromList pairs)
  Just _ -> Left "'paths' must be an object"
  Nothing -> Left "Missing 'paths' key"

-- | Extract all methods from a single path item.
extractPathMethods ::
  Map.Map Text Value ->
  Text ->
  Value ->
  [(RouteKey, RouteInfo)]
extractPathMethods schemas path = \case
  Object methodsObj ->
    [ (rk, ri)
    | (methodKey, methodVal) <- KM.toList methodsObj,
      let methodText = Key.toText methodKey,
      Just rk <- [routeKeyFromPath path methodText],
      let ri = extractRouteInfo schemas methodVal
    ]
  _ -> []

-- | Extract route info (params, body, responses) from an operation object.
extractRouteInfo :: Map.Map Text Value -> Value -> RouteInfo
extractRouteInfo schemas = \case
  Object op ->
    let params = extractParams op
        body = extractRequestBody schemas op
        resps = extractResponses schemas op
        opId = case KM.lookup "operationId" op of
          Just (String t) -> Just t
          _ -> Nothing
     in RouteInfo
          { operationId = opId,
            queryParams = fst params,
            requiredQueryParams = snd params,
            requestBody = body,
            responses = resps
          }
  _ ->
    RouteInfo
      { operationId = Nothing,
        queryParams = Set.empty,
        requiredQueryParams = Set.empty,
        requestBody = Nothing,
        responses = Map.empty
      }

-- | Extract query parameters: (all query param names, required query param names).
extractParams :: KM.KeyMap Value -> (Set.Set Text, Set.Set Text)
extractParams op = case KM.lookup "parameters" op of
  Just (Array arr) ->
    let queryPs =
          [ (name, isReq)
          | Object p <- Vector.toList arr,
            String loc <- [fromMaybe Null (KM.lookup "in" p)],
            loc == "query",
            String name <- [fromMaybe Null (KM.lookup "name" p)],
            let isReq = case KM.lookup "required" p of
                  Just (Bool True) -> True
                  _ -> False
          ]
     in ( Set.fromList (map fst queryPs),
          Set.fromList [n | (n, True) <- queryPs]
        )
  _ -> (Set.empty, Set.empty)

-- | Extract and resolve the request body schema.
extractRequestBody :: Map.Map Text Value -> KM.KeyMap Value -> Maybe ResolvedSchema
extractRequestBody schemas op = do
  Object rb <- KM.lookup "requestBody" op
  Object content <- KM.lookup "content" rb
  -- Take the first content type's schema
  (_, contentVal) <- listToMaybe (KM.toList content)
  case contentVal of
    Object ct -> do
      schemaVal <- KM.lookup "schema" ct
      Just (resolveSchema schemas Set.empty schemaVal)
    _ -> Nothing

-- | Extract and resolve response schemas keyed by status code.
extractResponses :: Map.Map Text Value -> KM.KeyMap Value -> Map.Map Text ResolvedSchema
extractResponses schemas op = case KM.lookup "responses" op of
  Just (Object respsObj) ->
    Map.fromList
      [ (Key.toText code, resolveSchema schemas Set.empty schemaVal)
      | (code, respVal) <- KM.toList respsObj,
        Object resp <- [respVal],
        Object content <- [fromMaybe Null (KM.lookup "content" resp)],
        (_, contentVal) <- take 1 (KM.toList content),
        Object ct <- [contentVal],
        schemaVal <- maybeToList (KM.lookup "schema" ct)
      ]
  _ -> Map.empty

-- | Resolve a schema value, following $ref pointers and merging allOf.
-- The 'visited' set prevents infinite recursion on circular references.
resolveSchema :: Map.Map Text Value -> Set.Set Text -> Value -> ResolvedSchema
resolveSchema schemas visited = \case
  Object obj
    | Just (String ref) <- KM.lookup "$ref" obj ->
        resolveRef schemas visited ref
    | Just (Array allOfArr) <- KM.lookup "allOf" obj ->
        mergeAllOf schemas visited (Vector.toList allOfArr)
    | Just (Array oneOfArr) <- KM.lookup "oneOf" obj ->
        -- For oneOf, we take the union of all variants' properties.
        -- This is conservative: if any variant has a field, we track it.
        mergeAllOf schemas visited (Vector.toList oneOfArr)
    | otherwise ->
        resolveInlineSchema schemas visited obj
  _ -> emptySchema

-- | Resolve a $ref pointer like "#/components/schemas/Foo".
resolveRef :: Map.Map Text Value -> Set.Set Text -> Text -> ResolvedSchema
resolveRef schemas visited ref =
  let schemaName = last (Text.splitOn "/" ref)
   in if Set.member schemaName visited
        then emptySchema
        else case Map.lookup schemaName schemas of
          Just val -> resolveSchema schemas (Set.insert schemaName visited) val
          Nothing -> emptySchema

-- | Merge multiple schemas from an allOf array.
mergeAllOf :: Map.Map Text Value -> Set.Set Text -> [Value] -> ResolvedSchema
mergeAllOf schemas visited vals =
  let resolved = map (resolveSchema schemas visited) vals
   in foldl' mergeSchemas emptySchema resolved

-- | Merge two resolved schemas, combining required fields and properties.
mergeSchemas :: ResolvedSchema -> ResolvedSchema -> ResolvedSchema
mergeSchemas a b =
  ResolvedSchema
    { requiredFields = a.requiredFields <> b.requiredFields,
      properties = Map.union a.properties b.properties,
      enumValues = case (a.enumValues, b.enumValues) of
        (Nothing, x) -> x
        (x, Nothing) -> x
        (Just x, Just y) -> Just (Set.union x y),
      schemaType = a.schemaType <|> b.schemaType
    }

-- | Resolve an inline schema object (with properties, required, enum, type).
resolveInlineSchema ::
  Map.Map Text Value ->
  Set.Set Text ->
  KM.KeyMap Value ->
  ResolvedSchema
resolveInlineSchema schemas visited obj =
  let reqFields = case KM.lookup "required" obj of
        Just (Array arr) ->
          Set.fromList [t | String t <- Vector.toList arr]
        _ -> Set.empty

      props = case KM.lookup "properties" obj of
        Just (Object propsObj) ->
          Map.fromList
            [ (Key.toText k, resolveSchema schemas visited v)
            | (k, v) <- KM.toList propsObj
            ]
        _ -> Map.empty

      enumVals = case KM.lookup "enum" obj of
        Just (Array arr) ->
          Just (Set.fromList [t | String t <- Vector.toList arr])
        _ -> Nothing

      typ = case KM.lookup "type" obj of
        Just (String t) -> Just t
        _ -> Nothing
   in ResolvedSchema
        { requiredFields = reqFields,
          properties = props,
          enumValues = enumVals,
          schemaType = typ
        }
