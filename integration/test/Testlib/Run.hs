module Testlib.Run (main, mainI) where

import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Time.Clock
import RunAllTests
import System.Directory
import System.Environment
import System.FilePath
import Testlib.App
import Testlib.Assertions
import Testlib.Env
import Testlib.JSON
import Testlib.Options
import Testlib.Printing
import Testlib.Types
import Text.Printf
import UnliftIO.Async

data TestReport = TestReport
  { count :: Int,
    failures :: [String]
  }
  deriving (Eq, Show)

instance Semigroup TestReport where
  TestReport s1 f1 <> TestReport s2 f2 = TestReport (s1 + s2) (f1 <> f2)

instance Monoid TestReport where
  mempty = TestReport 0 mempty

runTest :: GlobalEnv -> App a -> IO (Either String a)
runTest ge action = lowerCodensity $ do
  env <- mkEnv ge
  liftIO $
    (Right <$> runAppWithEnv env action)
      `E.catches` [ E.Handler -- AssertionFailure
                      (fmap Left . printFailureDetails),
                    E.Handler
                      (fmap Left . printExceptionDetails)
                  ]

pluralise :: Int -> String -> String
pluralise 1 x = x
pluralise _ x = x <> "s"

printReport :: TestReport -> IO ()
printReport report = do
  unless (null report.failures) $ putStrLn $ "----------"
  putStrLn $ show report.count <> " " <> pluralise report.count "test" <> " run."
  unless (null report.failures) $ do
    putStrLn ""
    let numFailures = length report.failures
    putStrLn $ colored red (show numFailures <> " failed " <> pluralise numFailures "test" <> ": ")
    for_ report.failures $ \name ->
      putStrLn $ " - " <> name

testFilter :: TestOptions -> String -> Bool
testFilter opts n = included n && not (excluded n)
  where
    included name =
      (null opts.includeTests) || any (\x -> x `isInfixOf` name) opts.includeTests
    excluded name = any (\x -> x `isInfixOf` name) opts.excludeTests

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

  genv0 <- mkGlobalEnv cfg

  -- save removal key to a file
  genv <- lowerCodensity $ do
    env <- mkEnv genv0
    liftIO . runAppWithEnv env $ do
      config <- readServiceConfig Galley
      relPath <- config %. "settings.mlsPrivateKeyPaths.removal.ed25519" & asString
      path <-
        asks (.servicesCwdBase) <&> \case
          Nothing -> relPath
          Just dir -> dir </> "galley" </> relPath
      pure genv0 {gRemovalKeyPath = path}

  withAsync displayOutput $ \displayThread -> do
    report <- fmap mconcat $ pooledForConcurrently tests $ \(name, action) -> do
      if f name
        then do
          (mErr, tm) <- withTime (runTest genv action)
          case mErr of
            Left err -> do
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
            Right _ -> do
              writeOutput $ name <> colored green " OK" <> " (" <> printTime tm <> ")" <> "\n"
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
