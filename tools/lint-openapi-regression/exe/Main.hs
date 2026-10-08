module Main (main) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Imports
import LintOpenAPI.Baseline (discoverBaselineFiles, expectVersion, selectBaselineFile)
import LintOpenAPI.Compare (compareSpecs)
import LintOpenAPI.Ignore (isIgnored, readIgnoreFile, updateIgnoreMap, writeIgnoreFile)
import LintOpenAPI.Parse (parseOpenAPIFile)
import LintOpenAPI.Report (formatViolations)
import LintOpenAPI.Types (OpenAPISpec (..))
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import System.IO qualified as IO

-- | CLI configuration.
data Options = Options
  { baselineDir :: FilePath,
    inputFile :: FilePath,
    ignoreFile :: Maybe FilePath,
    updateIgnore :: Bool,
    apiVersion :: Maybe Int
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
    <*> optional
      ( option
          auto
          ( long "api-version"
              <> metavar "N"
              <> help "Compare only against the saved baseline of this version instead of all baselines"
          )
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
  baselineFiles <- case opts.apiVersion of
    Nothing -> do
      files <- discoverBaselineFiles opts.baselineDir
      when (null files) $
        Text.IO.hPutStrLn IO.stderr $
          "Warning: No baseline swagger-v*.json files found in " <> Text.pack opts.baselineDir
      pure files
    Just n -> do
      files <- discoverBaselineFiles opts.baselineDir
      case selectBaselineFile n files of
        Left err -> do
          Text.IO.hPutStrLn IO.stderr $ "Error: " <> Text.pack err
          exitWith (ExitFailure 2)
        Right fp -> pure [fp]

  -- When targeting a version, verify the candidate actually is that version
  case opts.apiVersion of
    Just n -> do
      let candidateVersion = case candidate of OpenAPISpec {version = v} -> v
      case candidateVersion of
        Nothing -> do
          Text.IO.hPutStrLn IO.stderr "Error: Candidate spec has no version (servers[0].url)"
          exitWith (ExitFailure 2)
        Just m
          | m /= n -> do
              Text.IO.hPutStrLn IO.stderr $
                "Error: Candidate spec is v"
                  <> Text.pack (show m)
                  <> " but --api-version v"
                  <> Text.pack (show n)
                  <> " was requested"
              exitWith (ExitFailure 2)
          | otherwise -> pure ()
    Nothing -> pure ()
  baselines <- forM baselineFiles $ \fp -> do
    result <- parseOpenAPIFile fp
    case result of
      Left err -> case opts.apiVersion of
        Just _ ->
          Text.IO.hPutStrLn IO.stderr ("Error: Failed to parse " <> Text.pack fp <> ": " <> Text.pack err)
            >> exitWith (ExitFailure 2)
        Nothing -> do
          Text.IO.hPutStrLn IO.stderr $ "Warning: Failed to parse " <> Text.pack fp <> ": " <> Text.pack err
          pure Nothing
      Right spec -> do
        case opts.apiVersion of
          Just n -> case expectVersion n spec of
            Left err -> do
              Text.IO.hPutStrLn IO.stderr $
                "Error: " <> Text.pack fp <> ": " <> Text.pack err
              exitWith (ExitFailure 2)
            Right () -> pure ()
          Nothing -> pure ()
        pure (Just spec)

  when (isJust opts.apiVersion) $
    forM_ opts.ignoreFile $ \f ->
      Text.IO.hPutStrLn IO.stderr $
        "Warning: ignore entries are keyed by baseline version and record intentional "
          <> "changes since that version, so they may mask frozen-version drift in "
          <> Text.pack f
          <> "; use a separate drift ignore file instead"

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
