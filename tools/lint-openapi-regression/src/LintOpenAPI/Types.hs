module LintOpenAPI.Types
  ( RouteSegment (..),
    NormalizedRoute (..),
    HttpMethod (..),
    RouteKey (..),
    ResolvedSchema (..),
    RouteInfo (..),
    Violation (..),
    ViolationContext (..),
    OpenAPISpec (..),
    emptySchema,
  )
where

import Imports

-- | A segment of a URL path, either a literal string or a placeholder.
data RouteSegment
  = Literal Text
  | Placeholder
  deriving stock (Eq, Ord, Show, Generic)

-- | A normalized route path where all placeholders are unified.
newtype NormalizedRoute = NormalizedRoute
  { segments :: [RouteSegment]
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | HTTP methods relevant for OpenAPI comparison.
data HttpMethod
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Unique identity of a route across API versions.
data RouteKey = RouteKey
  { method :: HttpMethod,
    route :: NormalizedRoute
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | A resolved (flattened) schema with all $ref pointers followed.
data ResolvedSchema = ResolvedSchema
  { requiredFields :: Set Text,
    properties :: Map Text ResolvedSchema,
    enumValues :: Maybe (Set Text),
    schemaType :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | An empty schema with no fields, no enum, no type.
emptySchema :: ResolvedSchema
emptySchema =
  ResolvedSchema
    { requiredFields = mempty,
      properties = mempty,
      enumValues = Nothing,
      schemaType = Nothing
    }

-- | All information about a route needed for backward-compat checking.
data RouteInfo = RouteInfo
  { operationId :: Maybe Text,
    queryParams :: Set Text,
    requiredQueryParams :: Set Text,
    requestBody :: Maybe ResolvedSchema,
    responses :: Map Text ResolvedSchema
  }
  deriving stock (Eq, Show, Generic)

-- | A parsed OpenAPI specification.
data OpenAPISpec = OpenAPISpec
  { version :: Maybe Int,
    routes :: Map RouteKey RouteInfo
  }
  deriving stock (Eq, Show, Generic)

-- | A specific backward-incompatible change detected.
data Violation
  = RouteRemoved
  | QueryParamRemoved Text
  | RequiredQueryParamAdded Text
  | RequiredBodyFieldAdded Text
  | ResponseFieldRemoved Text
  | EnumValueRemoved Text Text
  | EnumValueAdded Text Text
  deriving stock (Eq, Show, Generic)

-- | A violation together with its context for reporting.
data ViolationContext = ViolationContext
  { baselineVersion :: Maybe Int,
    routeKey :: RouteKey,
    routeId :: Maybe Text,
    violation :: Violation
  }
  deriving stock (Eq, Show, Generic)
