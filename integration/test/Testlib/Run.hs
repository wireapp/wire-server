module Testlib.Run (main, mainI) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List
import RunAllTests
import System.Directory
import System.Environment
import Testlib.App
import Testlib.Options

data TestReport = TestReport
  { count :: Int,
    failures :: [String]
  }
  deriving (Eq, Show)

instance Semigroup TestReport where
  TestReport s1 f1 <> TestReport s2 f2 = TestReport (s1 + s2) (f1 <> f2)

instance Monoid TestReport where
  mempty = TestReport 0 mempty

printReport :: TestReport -> IO ()
printReport report = do
  unless (null report.failures) $ putStrLn $ "----------"
  putStrLn $ show report.count <> " tests run"
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
      cfg = "services/integration.yaml"
  output <- newChan
  let displayOutput =
        readChan output >>= \case
          Just x -> putStr x *> displayOutput
          Nothing -> pure ()
  let writeOutput = writeChan output . Just

  f <- testFilter <$> getOptions
  withAsync displayOutput $ \displayThread -> do
    report <- fmap mconcat $ forConcurrently tests $ \(name, action) -> do
      if (f name)
        then do
          mErr <- runTest cfg action
          case mErr of
            Just err -> do
              writeOutput $
                "----- " <> name <> colored red " FAIL" <> " -----\n" <> err <> "\n"
              pure (TestReport 1 [name])
            Nothing -> do
              writeOutput $ name <> colored green " \x2713" <> "\n"
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
