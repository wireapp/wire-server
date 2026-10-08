module LintOpenAPI.Ignore
  ( IgnoreMap,
    readIgnoreFile,
    writeIgnoreFile,
    isIgnored,
    updateIgnoreMap,
    getRouteIdentifier,
  )
where

import Data.Aeson (decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Imports
import LintOpenAPI.Report (renderRouteKey)
import LintOpenAPI.Types

-- | Maps a baseline version string (e.g. "v7") to a set of route identifiers.
type IgnoreMap = Map.Map Text (Set.Set Text)

-- | Read an ignore file. Returns an empty map if it doesn't exist or is invalid.
readIgnoreFile :: FilePath -> IO IgnoreMap
readIgnoreFile path = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else do
      bytes <- BS.readFile path
      case decodeStrict bytes of
        Just m -> pure m
        Nothing -> pure Map.empty

-- | Write an ignore file using pretty JSON formatting.
writeIgnoreFile :: FilePath -> IgnoreMap -> IO ()
writeIgnoreFile path imap = do
  let bytes = encodePretty imap
  LBS.writeFile path bytes

-- | Extract the identifier for a route (either its operationId or rendered path).
getRouteIdentifier :: ViolationContext -> Text
getRouteIdentifier ctx =
  fromMaybe (renderRouteKey ctx.routeKey) ctx.routeId

-- | Format the version key for the ignore map.
versionKey :: Maybe Int -> Text
versionKey (Just v) = "v" <> Text.pack (show v)
versionKey Nothing = "baseline"

-- | Check if a violation is ignored by the given ignore map.
isIgnored :: IgnoreMap -> ViolationContext -> Bool
isIgnored imap ctx =
  let vkey = versionKey ctx.baselineVersion
      rkey = getRouteIdentifier ctx
   in case Map.lookup vkey imap of
        Just ignoredRoutes -> Set.member rkey ignoredRoutes
        Nothing -> False

-- | Update the ignore map with new unignored violations.
updateIgnoreMap :: IgnoreMap -> [ViolationContext] -> IgnoreMap
updateIgnoreMap imap newViolations =
  foldl' addViolation imap newViolations
  where
    addViolation acc ctx =
      let vkey = versionKey ctx.baselineVersion
          rkey = getRouteIdentifier ctx
       in Map.insertWith Set.union vkey (Set.singleton rkey) acc
