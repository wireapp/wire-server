module LintOpenAPI.Compare
  ( compareSpecs,
    compareRouteInfo,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Imports
import LintOpenAPI.Types

-- | Compare a candidate spec against a baseline spec.
-- Returns violations where the candidate breaks backward compatibility.
compareSpecs :: OpenAPISpec -> OpenAPISpec -> [ViolationContext]
compareSpecs baseline candidate =
  concatMap (uncurry $ compareRoute baseline.version candidate.routes) (Map.toList baseline.routes)

-- | Compare a single baseline route against the candidate route map.
compareRoute ::
  Maybe Int ->
  Map.Map RouteKey RouteInfo ->
  RouteKey ->
  RouteInfo ->
  [ViolationContext]
compareRoute ver candidateRoutes rk baselineInfo =
  case Map.lookup rk candidateRoutes of
    Nothing ->
      [mkCtx RouteRemoved]
    Just candidateInfo ->
      map mkCtx (compareRouteInfo baselineInfo candidateInfo)
  where
    mkCtx v =
      ViolationContext
        { baselineVersion = ver,
          routeKey = rk,
          routeId = baselineInfo.operationId,
          violation = v
        }

-- | Compare route info for backward-incompatible changes.
compareRouteInfo :: RouteInfo -> RouteInfo -> [Violation]
compareRouteInfo baseline candidate =
  compareQueryParams baseline candidate
    <> compareRequestBody baseline candidate
    <> compareResponseBodies baseline candidate

-- | Check for removed query params and new required query params.
compareQueryParams :: RouteInfo -> RouteInfo -> [Violation]
compareQueryParams baseline candidate =
  let removedParams = Set.difference baseline.queryParams candidate.queryParams
      newRequiredParams = Set.difference candidate.requiredQueryParams baseline.queryParams
   in map QueryParamRemoved (Set.toList removedParams)
        <> map RequiredQueryParamAdded (Set.toList newRequiredParams)

-- | Check for new required fields in the request body.
compareRequestBody :: RouteInfo -> RouteInfo -> [Violation]
compareRequestBody baseline candidate =
  case (baseline.requestBody, candidate.requestBody) of
    (_, Nothing) -> []
    (Nothing, Just candSchema) ->
      -- Candidate added a body where none existed; only flag required fields
      map RequiredBodyFieldAdded (Set.toList candSchema.requiredFields)
    (Just baseSchema, Just candSchema) ->
      compareRequestSchema baseSchema candSchema

-- | Compare request schemas for new required fields and removed enum values.
compareRequestSchema :: ResolvedSchema -> ResolvedSchema -> [Violation]
compareRequestSchema baseline candidate =
  let newRequired = Set.difference candidate.requiredFields baseline.requiredFields
      enumViolations = compareRequestEnumValues baseline candidate
      nestedViolations = compareNestedRequestSchemas baseline candidate
   in map RequiredBodyFieldAdded (Set.toList newRequired)
        <> enumViolations
        <> nestedViolations

-- | Compare response bodies for removed required fields.
compareResponseBodies :: RouteInfo -> RouteInfo -> [Violation]
compareResponseBodies baseline candidate =
  concatMap checkResponse (Map.toList baseline.responses)
  where
    checkResponse (code, baseSchema) =
      case Map.lookup code candidate.responses of
        Nothing -> []
        Just candSchema -> compareResponseSchema baseSchema candSchema

-- | Compare response schemas for removed fields and removed enum values.
compareResponseSchema :: ResolvedSchema -> ResolvedSchema -> [Violation]
compareResponseSchema baseline candidate =
  let baseRequiredProps = Set.intersection baseline.requiredFields (Map.keysSet baseline.properties)
      candProps = Map.keysSet candidate.properties
      removedRequiredProps = Set.difference baseRequiredProps candProps
      enumViolations = compareResponseEnumValues baseline candidate
      nestedViolations = compareNestedResponseSchemas baseline candidate
   in map ResponseFieldRemoved (Set.toList removedRequiredProps)
        <> enumViolations
        <> nestedViolations

-- | In request schemas, removing an enum value is breaking (clients may send it).
compareRequestEnumValues :: ResolvedSchema -> ResolvedSchema -> [Violation]
compareRequestEnumValues baseline candidate =
  case (baseline.enumValues, candidate.enumValues) of
    (Just baseEnum, Just candEnum) ->
      let removed = Set.difference baseEnum candEnum
       in map (EnumValueRemoved "") (Set.toList removed)
    _ -> []

-- | In response schemas, adding an enum value is breaking (clients may not handle it).
compareResponseEnumValues :: ResolvedSchema -> ResolvedSchema -> [Violation]
compareResponseEnumValues baseline candidate =
  case (baseline.enumValues, candidate.enumValues) of
    (Just baseEnum, Just candEnum) ->
      let added = Set.difference candEnum baseEnum
       in map (EnumValueAdded "") (Set.toList added)
    _ -> []

-- | Recursively compare nested request schemas for properties that exist in both.
compareNestedRequestSchemas :: ResolvedSchema -> ResolvedSchema -> [Violation]
compareNestedRequestSchemas baseline candidate =
  let commonProps = Map.intersectionWith (,) baseline.properties candidate.properties
   in concatMap
        (\(_, (baseProp, candProp)) -> compareRequestSchema baseProp candProp)
        (Map.toList commonProps)

-- | Recursively compare nested response schemas for properties that exist in both.
compareNestedResponseSchemas :: ResolvedSchema -> ResolvedSchema -> [Violation]
compareNestedResponseSchemas baseline candidate =
  let commonProps = Map.intersectionWith (,) baseline.properties candidate.properties
   in concatMap
        (\(_, (baseProp, candProp)) -> compareResponseSchema baseProp candProp)
        (Map.toList commonProps)
