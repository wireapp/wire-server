module Main (main) where

import Data.Aeson (decode')
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Imports
import LintOpenAPI.Baseline (discoverBaselineFiles, expectVersion, extractVersionNum, selectBaselineFile)
import LintOpenAPI.Compare (compareSpecs)
import LintOpenAPI.Fetch (fetchVersionSpec)
import LintOpenAPI.Ignore (IgnoreSet, collectIgnoreSet, isIgnored, readIgnoreFile, writeIgnoreFile)
import LintOpenAPI.Parse (parseOpenAPIFile, parseOpenAPIValue)
import LintOpenAPI.Report (formatViolations)
import LintOpenAPI.Types (OpenAPISpec (..), ViolationContext (..))
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO qualified as IO

-- | CLI configuration.
data Options = Options
  { baselineDir :: FilePath,
    -- | Optional offline candidate spec; otherwise fetched from brig.
    inputFile :: Maybe FilePath,
    ignoreFile :: Maybe FilePath,
    updateIgnore :: Bool,
    brigUrl :: Text,
    ignoreDir :: FilePath,
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
    <*> optional
      ( strArgument
          ( metavar "INPUT_FILE"
              <> help "Path to an OpenAPI JSON file to check (only with --api-version); otherwise specs are fetched from brig"
          )
      )
    <*> optional
      ( strOption
          ( long "ignore"
              <> metavar "FILE"
              <> help "Path to a flat JSON ignore file (only with --api-version)"
          )
      )
    <*> switch
      ( long "update"
          <> help "Write the current violations to the ignore file(s)"
      )
    <*> strOption
      ( long "brig-url"
          <> metavar "URL"
          <> value "http://localhost:8082"
          <> showDefault
          <> help "Base URL of a running brig used to fetch live per-version specs"
      )
    <*> strOption
      ( long "ignore-dir"
          <> metavar "DIR"
          <> value ".lint-ignore"
          <> showDefault
          <> help "Directory holding per-version ignore files vX.json (used by --update without --ignore)"
      )
    <*> optional
      ( option
          auto
          ( long "api-version"
              <> metavar "N"
              <> help "Check only version N: baseline vX vs the spec served for vN (or INPUT_FILE)"
          )
      )

-- | Top-level CLI parser with help text.
optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Check OpenAPI specs for backward-incompatible changes: snapshot-vs-live for \
          \one version (--api-version) or for every baseline version"
        <> header "lint-openapi-regression - OpenAPI backward-compatibility linter"
    )

main :: IO ()
main = do
  opts <- execParser optionsParserInfo
  result <- runLint opts
  exitWith result

failWith :: Text -> IO a
failWith msg = do
  Text.IO.hPutStrLn IO.stderr ("Error: " <> msg)
  exitWith (ExitFailure 2)

-- | Fetch the live spec for version @n@ from brig and parse it.
fetchAndParse :: Text -> Int -> IO OpenAPISpec
fetchAndParse brigUrl n = do
  fetched <- fetchVersionSpec brigUrl n
  body <- either (failWith . Text.pack) pure fetched
  case decode' body of
    Nothing -> failWith ("Failed to parse the spec fetched for v" <> Text.pack (show n))
    Just val -> either (failWith . Text.pack) pure (parseOpenAPIValue val)

-- | Parse one baseline snapshot, failing with the file name in the message.
readBaseline :: FilePath -> IO OpenAPISpec
readBaseline fp =
  parseOpenAPIFile fp
    >>= either (failWith . Text.pack . (("Failed to parse " <> fp <> ": ") <>)) pure

-- | Main lint workflow. Returns appropriate exit code.
runLint :: Options -> IO ExitCode
runLint opts = case opts.apiVersion of
  Just n -> runSingleVersion opts n
  Nothing -> runAllVersions opts

-- | Single-version mode: baseline vN against INPUT_FILE or the live spec of vN.
runSingleVersion :: Options -> Int -> IO ExitCode
runSingleVersion opts n = do
  candidate <- case opts.inputFile of
    Just fp -> do
      exists <- doesFileExist fp
      unless exists $
        failWith ("Input file not found: " <> Text.pack fp)
      result <- parseOpenAPIFile fp
      either (failWith . Text.pack) pure result
    Nothing -> fetchAndParse opts.brigUrl n
  either (failWith . Text.pack) pure (expectVersion n candidate)
  files <- discoverBaselineFiles opts.baselineDir
  fp <- either (failWith . Text.pack) pure (selectBaselineFile n files)
  baseline <- readBaseline fp
  either (failWith . Text.pack) pure (expectVersion n baseline)
  ignoreSet <- loadIgnoreSet opts
  let violations = compareSpecs baseline candidate
      (ignored, unignored) = List.partition (isIgnored ignoreSet) violations
  Text.IO.hPutStrLn IO.stderr (formatViolations (length ignored) unignored)
  when opts.updateIgnore $
    case opts.ignoreFile of
      Just f -> writeIgnoreTarget f violations
      Nothing -> writeIgnoreTarget (opts.ignoreDir </> ("v" <> show n <> ".json")) violations
  pure (if null unignored then ExitSuccess else ExitFailure 1)

-- | All-versions mode: every baseline snapshot vs the live spec of that version.
runAllVersions :: Options -> IO ExitCode
runAllVersions opts = do
  when (isJust opts.inputFile) $
    failWith "INPUT_FILE requires --api-version"
  when (isJust opts.ignoreFile) $
    failWith "--ignore requires --api-version"
  files <- discoverBaselineFiles opts.baselineDir
  when (null files) $
    Text.IO.hPutStrLn IO.stderr $
      "Warning: No baseline swagger-v*.json files found in " <> Text.pack opts.baselineDir
  perVersion <- forM files $ \fp -> do
    baseline <- readBaseline fp
    n <- case extractVersionNum (takeFileName fp) of
      Just v -> pure v
      Nothing -> failWith ("Cannot determine version from baseline file name: " <> Text.pack fp)
    either (failWith . Text.pack) pure (expectVersion n baseline)
    live <- fetchAndParse opts.brigUrl n
    pure (n, compareSpecs baseline live)
  let sorted = List.sortOn fst perVersion
      allCtxs = concatMap snd sorted
  Text.IO.hPutStrLn IO.stderr (formatViolations 0 allCtxs)
  when opts.updateIgnore $ do
    createDirectoryIfMissing True opts.ignoreDir
    forM_ sorted $ \(n, ctxs) ->
      unless (null ctxs) $
        writeIgnoreTarget (opts.ignoreDir </> ("v" <> show n <> ".json")) ctxs
  pure (if null allCtxs then ExitSuccess else ExitFailure 1)

-- | Load the explicit @--ignore@ set.  There is no implicit ignore file.
loadIgnoreSet :: Options -> IO IgnoreSet
loadIgnoreSet opts = case opts.ignoreFile of
  Nothing -> pure mempty
  Just f -> either (failWith . Text.pack) pure =<< readIgnoreFile f

-- | Write the full current violation set of one version to a single file.
writeIgnoreTarget :: FilePath -> [ViolationContext] -> IO ()
writeIgnoreTarget f ctxs = do
  createDirectoryIfMissing True (takeDirectory f)
  writeIgnoreFile f (collectIgnoreSet ctxs)
  Text.IO.hPutStrLn IO.stderr ("Updated ignore file: " <> Text.pack f)
