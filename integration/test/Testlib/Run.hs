module Testlib.Run (main, mainI) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List
import Data.Time.Clock
import RunAllTests
import System.Directory
import System.Environment
import System.IO
import Testlib.App
import Testlib.Options
import Text.Printf
import UnliftIO.Async

-- TODO
--
-- [x] Limit concurrency to the number of capabilities
-- [x] Parse configuration before running tests
-- [x] Add -c option for the configuration directory
-- [ ] Clean up temporary files

data TestReport = TestReport
  { count :: Int,
    failures :: [String]
  }
  deriving (Eq, Show)

instance Semigroup TestReport where
  TestReport s1 f1 <> TestReport s2 f2 = TestReport (s1 + s2) (f1 <> f2)

instance Monoid TestReport where
  mempty = TestReport 0 mempty

pluralise :: Int -> String -> String
pluralise 1 x = x
pluralise _ x = x <> "s"

printReport :: TestReport -> IO ()
printReport report = do
  unless (null report.failures) $ putStrLn $ "----------"
  putStrLn $ show report.count <> " " <> pluralise report.count "test" <> " run."
  unless (null report.failures) $ do
    putStrLn $ colored red "\nFailed tests: "
    for_ report.failures $ \name ->
      putStrLn $ " - " <> name

testFilter :: TestOptions -> String -> Bool
testFilter opts n = included n && not (excluded n)
  where
    included name =
      if null opts.includeTests
        then True
        else any (\x -> isInfixOf x name) opts.includeTests
    excluded name = any (\x -> isInfixOf x name) opts.excludeTests

withTime :: IO a -> IO (a, NominalDiffTime)
withTime action = do
  tm0 <- getCurrentTime
  a <- action
  tm1 <- getCurrentTime
  pure (a, diffUTCTime tm1 tm0)

printTime :: NominalDiffTime -> String
printTime =
  printf "%.02f s"
    . (/ 100.0)
    . (fromIntegral :: Integer -> Float)
    . truncate
    . (* 100)
    . nominalDiffTimeToSeconds

main :: IO ()
main = do
  let tests =
        sortOn fst $
          allTests <&> \(module_, name, _summary, _full, action) ->
            let module0 = case module_ of
                  ('T' : 'e' : 's' : 't' : '.' : m) -> m
                  _ -> module_
                qualifiedName = module0 <> "." <> name
             in (qualifiedName, action)
  output <- newChan
  let displayOutput =
        readChan output >>= \case
          Just x -> putStr x *> displayOutput
          Nothing -> pure ()
  let writeOutput = writeChan output . Just

  opts <- getOptions
  let f = testFilter opts
      cfg = opts.configFile

  env <- mkGlobalEnv cfg

  withAsync displayOutput $ \displayThread -> do
    report <- fmap mconcat $ pooledForConcurrently tests $ \(name, action) -> do
      if (f name)
        then do
          (mErr, tm) <- withTime (runTest env action)
          case mErr of
            Just err -> do
              writeOutput $
                "----- "
                  <> name
                  <> colored red " FAIL"
                  <> " ("
                  <> printTime tm
                  <> ") -----\n"
                  <> err
                  <> "\n"
              pure (TestReport 1 [name])
            Nothing -> do
              writeOutput $ name <> colored green " \x2713" <> " (" <> printTime tm <> ")" <> "\n"
              pure (TestReport 1 [])
        else pure (TestReport 0 [])
    writeChan output Nothing
    wait displayThread
    printReport report

-- like `main` but meant to run from a repl
mainI :: [String] -> IO ()
mainI args = do
  let projectRoot = "../"
  withArgs args $
    withCurrentDirectory projectRoot $
      main
