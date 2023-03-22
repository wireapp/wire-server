module Run (main) where

import Config
import qualified Data.Yaml as Yaml
import Imports
import RunAllTests
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)
import Test.Tasty (testGroup)
import Test.Tasty.Options (lookupOption, setOption)
import Test.Tasty.Providers (singleTest)
import Test.Tasty.Runners (consoleTestReporter, listingTests, parseOptions, tryIngredients)

main :: IO ()
main = do
  let ingredients = [listingTests, consoleTestReporter]

  let tree =
        testGroup "Tests" $
          allTests <&> \(module_, name, _summary, _full, action) ->
            let qualifiedName = module_ <> "." <> name
             in singleTest qualifiedName action

  optsCLI <- parseOptions ingredients tree

  let ConfigFile cfgFile = lookupOption @ConfigFile optsCLI

  eith <- Yaml.decodeFileEither cfgFile
  serviceMap <- case eith of
    Left err -> do
      hPutStrLn stderr $ "Could not parse " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err
      exitFailure
    Right serviceMap -> pure serviceMap

  let opts = setOption @ServiceMap serviceMap optsCLI

  case tryIngredients ingredients opts tree of
    Nothing -> do
      hPutStrLn
        stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure
