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
  putStrLn $ "----------"
  putStrLn $ show report.count <> " tests run\n"
  putStrLn $ colored red "Failed tests: "
  for_ report.failures $ \name ->
    putStrLn $ " - " <> name

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
  let displayOutput = forever $ do
        x <- readChan output
        putStr x
  withAsync displayOutput $ \displayThread -> do
    report <- fmap mconcat $ forConcurrently tests $ \(name, action) -> do
      mErr <- runTest cfg action
      case mErr of
        Just err -> do
          writeChan output $
            "----- " <> name <> colored red " FAIL" <> " -----\n" <> err <> "\n"
          pure (TestReport 1 [name])
        Nothing -> do
          writeChan output $ name <> colored green " \x2713" <> "\n"
          pure (TestReport 1 [])
    cancel displayThread
    printReport report

-- like `main` but meant to run from a repl
mainI :: [String] -> IO ()
mainI args = do
  let projectRoot = "../"
  withArgs args $
    withCurrentDirectory projectRoot $
      main
