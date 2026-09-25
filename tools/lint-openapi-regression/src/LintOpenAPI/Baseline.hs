module LintOpenAPI.Baseline
  ( discoverBaselineFiles,
    selectBaselineFile,
    expectVersion,
  )
where

import Imports
import LintOpenAPI.Types (OpenAPISpec (..))
import System.FilePath (takeFileName, (</>))

-- | Discover baseline swagger-v*.json files in a directory.
-- Only includes OpenAPI 3.0 files (v5+).
discoverBaselineFiles :: FilePath -> IO [FilePath]
discoverBaselineFiles dir = do
  exists <- doesFileExist (dir </> "swagger-v5.json")
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      let swaggerFiles =
            sort
              [ dir </> e
              | e <- entries,
                "swagger-v" `isPrefixOf` e,
                ".json" `isSuffixOf` e,
                isOpenAPI3File e
              ]
      pure swaggerFiles

-- | Check if a swagger filename is v5 or higher (OpenAPI 3.0).
isOpenAPI3File :: String -> Bool
isOpenAPI3File name =
  case extractVersionNum name of
    Just n -> n >= 5
    Nothing -> False

-- | Extract the version number from a filename like "swagger-v5.json".
extractVersionNum :: String -> Maybe Int
extractVersionNum name = do
  rest <- stripPrefix "swagger-v" name
  let numStr = takeWhile (/= '.') rest
  readMaybe numStr

-- | Pick the saved baseline file for a specific version among discovered baselines.
selectBaselineFile :: Int -> [FilePath] -> Either String FilePath
selectBaselineFile n files =
  case filter (\fp -> let name = takeFileName fp in name == expectedName && isOpenAPI3File name) files of
    (fp : _) -> Right fp
    [] ->
      Left
        $ "No saved baseline for version v"
        <> show n
        <> ": "
        <> expectedName
        <> " not found (only OpenAPI 3 baselines, v5+, are supported)"
  where
    expectedName = "swagger-v" <> show n <> ".json"

-- | Check that a spec carries the expected version (parsed from servers[0].url).
expectVersion :: Int -> OpenAPISpec -> Either String ()
expectVersion n spec = case spec.version of
  Nothing -> Left "spec has no version (servers[0].url)"
  Just m
    | m == n -> Right ()
    | otherwise -> Left $ "expected v" <> show n <> ", found v" <> show m
