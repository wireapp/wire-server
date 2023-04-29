{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Testlib.Run (main, mainI) where

import Imports
import RunAllTests
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)
import Test.Tasty (testGroup)
import Test.Tasty.Options (lookupOption, setOption)
import Test.Tasty.Providers (singleTest)
import Test.Tasty.Runners (TestPattern (..), consoleTestReporter, listingTests, parseOptions, tryIngredients)
import Testlib.Options

main :: IO ()
main = do
  let ingredients = [listingTests, consoleTestReporter]

  let tree =
        testGroup "Tests" $
          let ts =
                allTests <&> \(module_, name, _summary, _full, action) ->
                  let qualifiedName = module_ <> "." <> name
                   in (qualifiedName, action)
           in map (uncurry singleTest) (sortOn fst ts)

  optsCLI <- parseOptions ingredients tree

  let sel :: TestSelection = lookupOption optsCLI
  let optsCLI' = case convertToAwk sel of
        Nothing -> optsCLI
        Just expr -> setOption (TestPattern (Just expr)) optsCLI

  case tryIngredients ingredients optsCLI' tree of
    Nothing -> do
      hPutStrLn
        stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

-- like `main` but meant to run from a repl
mainI :: [String] -> IO ()
mainI args = do
  let projectRoot = "../"
  withArgs args $
    withCurrentDirectory projectRoot $
      main
