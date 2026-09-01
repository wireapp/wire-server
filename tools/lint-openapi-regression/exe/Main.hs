module Main (main) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Imports
import LintOpenAPI.Compare (compareSpecs)
import LintOpenAPI.Ignore (isIgnored, readIgnoreFile, updateIgnoreMap, writeIgnoreFile)
import LintOpenAPI.Parse (parseOpenAPIFile)
import LintOpenAPI.Report (formatViolations)
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO qualified as IO

-- | CLI configuration.
data Options = Options
  { baselineDir :: FilePath,
    inputFile :: FilePath,
    ignoreFile :: Maybe FilePath,
    updateIgnore :: Bool
  }

-- | Parse CLI options.
optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "baseline-dir"
          <> metavar "DIR"
          <> value "services/brig/docs"
          <> showDefault
          <> help "Directory containing baseline swagger-v*.json files"
      )
    <*> strArgument
      ( metavar "INPUT_FILE"
          <> help "Path to the new OpenAPI JSON file to check"
      )
    <*> optional
      ( strOption
          ( long "ignore"
              <> metavar "FILE"
              <> help "Path to the JSON ignore file"
          )
      )
    <*> switch
      ( long "update"
          <> help "Update the ignore file with unignored breaking changes"
      )

-- | Top-level CLI parser with help text.
optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Check an OpenAPI JSON file for backward-incompatible changes"
        <> header "lint-openapi-regression - OpenAPI backward-compatibility linter"
    )

main :: IO ()
main = do
  opts <- execParser optionsParserInfo
  result <- runLint opts
  exitWith result

-- | Main lint workflow. Returns appropriate exit code.
runLint :: Options -> IO ExitCode
runLint opts = do
  -- Parse the input file
  inputExists <- doesFileExist opts.inputFile
  unless inputExists $ do
    Text.IO.hPutStrLn IO.stderr $ "Error: Input file not found: " <> Text.pack opts.inputFile
    exitWith (ExitFailure 2)

  inputResult <- parseOpenAPIFile opts.inputFile
  candidate <- case inputResult of
    Left err -> do
      Text.IO.hPutStrLn IO.stderr $ "Error parsing input file: " <> Text.pack err
      exitWith (ExitFailure 2)
    Right spec -> pure spec

  -- Discover and parse baseline files
  baselineFiles <- discoverBaselineFiles opts.baselineDir
  when (null baselineFiles) $ do
    Text.IO.hPutStrLn IO.stderr $
      "Warning: No baseline swagger-v*.json files found in " <> Text.pack opts.baselineDir

  baselines <- forM baselineFiles $ \fp -> do
    result <- parseOpenAPIFile fp
    case result of
      Left err -> do
        Text.IO.hPutStrLn IO.stderr $ "Warning: Failed to parse " <> Text.pack fp <> ": " <> Text.pack err
        pure Nothing
      Right spec -> pure (Just spec)

  -- Load ignore map if specified
  ignoreMap <- case opts.ignoreFile of
    Just f -> readIgnoreFile f
    Nothing -> pure Map.empty

  let validBaselines = catMaybes baselines
      allViolations = concatMap (`compareSpecs` candidate) validBaselines
      (ignoredViolations, unignoredViolations) = List.partition (isIgnored ignoreMap) allViolations
      report = formatViolations (length ignoredViolations) unignoredViolations

  Text.IO.hPutStrLn IO.stderr report

  when opts.updateIgnore $ do
    case opts.ignoreFile of
      Just f -> do
        let newMap = updateIgnoreMap ignoreMap unignoredViolations
        writeIgnoreFile f newMap
        Text.IO.hPutStrLn IO.stderr $ "Updated ignore file: " <> Text.pack f
      Nothing -> Text.IO.hPutStrLn IO.stderr "Warning: --update specified but no --ignore file provided."

  if null unignoredViolations
    then pure ExitSuccess
    else pure (ExitFailure 1)

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
            List.sort
              [ dir </> e
              | e <- entries,
                "swagger-v" `List.isPrefixOf` e,
                ".json" `List.isSuffixOf` e,
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
  rest <- List.stripPrefix "swagger-v" name
  let numStr = takeWhile (/= '.') rest
  readMaybe numStr
