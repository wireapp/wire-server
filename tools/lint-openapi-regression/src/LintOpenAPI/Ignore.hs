module LintOpenAPI.Ignore
  ( IgnoreSet,
    readIgnoreFile,
    writeIgnoreFile,
    isIgnored,
    collectIgnoreSet,
    getRouteIdentifier,
  )
where

import Data.Aeson (decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Imports
import LintOpenAPI.Report (renderRouteKey)
import LintOpenAPI.Types

-- | A flat set of route identifiers (either an @operationId@ or a rendered
-- route like @"GET /users/{_}"@) whose violations are suppressed.
type IgnoreSet = Set Text

-- | Read a flat ignore file: a JSON array of strings.  Returns @Left@ if the
-- file is missing or malformed; there is no implicit empty ignore set.
readIgnoreFile :: FilePath -> IO (Either String IgnoreSet)
readIgnoreFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("ignore file not found: " <> path))
    else do
      bytes <- BS.readFile path
      pure $ case decodeStrict bytes :: Maybe [Text] of
        Just rs -> Right (Set.fromList rs)
        Nothing -> Left ("ignore file is not a JSON array of strings: " <> path)

-- | Write an ignore file as a sorted JSON array of strings.
writeIgnoreFile :: FilePath -> IgnoreSet -> IO ()
writeIgnoreFile path routes =
  LBS.writeFile path (encodePretty (Set.toAscList routes))

-- | Extract the identifier for a route (either its operationId or rendered path).
getRouteIdentifier :: ViolationContext -> Text
getRouteIdentifier ctx =
  fromMaybe (renderRouteKey ctx.routeKey) ctx.routeId

-- | Check if a violation is ignored by the given ignore set.
isIgnored :: IgnoreSet -> ViolationContext -> Bool
isIgnored ignored ctx = Set.member (getRouteIdentifier ctx) ignored

-- | Build the ignore set covering all given violations.
collectIgnoreSet :: [ViolationContext] -> IgnoreSet
collectIgnoreSet = Set.fromList . map getRouteIdentifier
