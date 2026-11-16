module LintOpenAPI.Normalize
  ( normalizePath,
    routeKeyFromPath,
  )
where

import Data.Text qualified as Text
import Imports
import LintOpenAPI.Types

-- | Normalize a URL path into a list of route segments.
-- Placeholders like @{userId}@ become 'Placeholder', everything else 'Literal'.
normalizePath :: Text -> NormalizedRoute
normalizePath path =
  NormalizedRoute
    { segments = map normalizeSegment (filter (not . Text.null) (Text.splitOn "/" path))
    }

-- | Normalize a single path segment.
normalizeSegment :: Text -> RouteSegment
normalizeSegment seg
  | Text.isPrefixOf "{" seg && Text.isSuffixOf "}" seg = Placeholder
  | otherwise = Literal seg

-- | Parse an HTTP method string into our enum.
parseMethod :: Text -> Maybe HttpMethod
parseMethod = \case
  "get" -> Just GET
  "post" -> Just POST
  "put" -> Just PUT
  "patch" -> Just PATCH
  "delete" -> Just DELETE
  "head" -> Just HEAD
  "options" -> Just OPTIONS
  _ -> Nothing

-- | Build a 'RouteKey' from a path string and method string.
routeKeyFromPath :: Text -> Text -> Maybe RouteKey
routeKeyFromPath path methodText = do
  m <- parseMethod (Text.toLower methodText)
  pure
    RouteKey
      { method = m,
        route = normalizePath path
      }
