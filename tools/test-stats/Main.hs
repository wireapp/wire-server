-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Main (main) where

import Control.Exception
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Database.PostgreSQL.Simple
import Imports hiding (Map)
import Options.Generic
import Prometheus
import Text.XML.Light

main :: IO ()
main = do
  opts :: Options Unwrapped <- unwrapRecord "Test Statistics"
  xmlFiles <- map ((opts.testResults <> "/") <>) . filter (".xml" `isSuffixOf`) <$> listDirectory opts.testResults
  reports <- mconcat <$> traverse parse xmlFiles
  pushToPostgresql opts reports
  LBS.putStrLn =<< exportMetricsAsText

-- * Command Line Options

data Options w = Options
  { testResults :: w ::: FilePath <?> "Directory containing test results in the JUNIT xml format. All files with 'xml' extension in this directory will be considered test results",
    time :: w ::: Maybe ZonedTime <?> "Time at which the test suite was run, defaults to current time.",
    runId :: w ::: Text <?> "ID of the flake-news run, this can be used to unite multiple test suites together",
    codeVersion :: w ::: Text <?> "Version of the code and tests, Plainly stored next to the stats so it can be used for some analysis",
    suiteName :: w ::: Text <?> "Name of the test suite, e.g brig, galley, integration, etc.",
    postgresqlHost :: w ::: String <?> "Hostname for postgresql DB" <!> "localhost",
    postgresqlPort :: w ::: Word16 <?> "Port for postgresql DB" <!> "5432",
    postgresqlDatabase :: w ::: String <?> "The database to use in postgresql DB" <!> "flake_news",
    postgresqlUser :: w ::: String <?> "Username for postgresql DB" <!> "flake_journalist",
    postgresqlPassword :: w ::: String <?> "Password for postgresql DB" <!> ""
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

-- * Intermediate representation

type TestCase = Text

data Report = Report
  { success :: Int,
    failure :: Int,
    failureDesc :: [Text]
  }
  deriving (Show)

instance Semigroup Report where
  a <> b =
    Report
      (a.success + b.success)
      (a.failure + b.failure)
      (a.failureDesc <> b.failureDesc)

successCase :: Report
successCase = Report 1 0 mempty

failureCase :: [Text] -> Report
failureCase = Report 0 1

-- * XML parser

-- | Returns (report by test case name, overallTestSuiteFailures, overallTestSuiteSucesses)
--
-- The overall numbers are always 1 or 0 and wrapped in the `Sum` type so we can
-- mconcat them to get the overall sum.
parse :: FilePath -> IO (MonoidalMap TestCase Report, Sum Int, Sum Int)
parse fn = do
  testCaseReport <- parseRaw <$> T.readFile fn
  if any (\r -> r.failure > 0) testCaseReport
    then pure (testCaseReport, Sum 1, Sum 0)
    else pure (testCaseReport, Sum 0, Sum 1)

parseRaw :: Text -> MonoidalMap TestCase Report
parseRaw = mconcat . map parseTestSuites . selectTestSuites . parseXML
  where
    selectTestSuites = filter (matchQName "testsuites" . elName) . onlyElems

parseTestSuites :: Element -> MonoidalMap TestCase Report
parseTestSuites = mconcat . liftM2 map (parseTestSuite . name) (children "testsuite")

parseTestSuite :: [Text] -> Element -> MonoidalMap TestCase Report
parseTestSuite names =
  liftM2
    (MonoidalMap.unionWith (<>))
    (mconcat . liftM2 map (parseTestSuite . (names <>) . name) (children "testsuite"))
    (mconcat . liftM2 map (parseTestCase . (names <>) . name) (children "testcase"))

parseTestCase :: [Text] -> Element -> MonoidalMap TestCase Report
parseTestCase names el
  | null failures = MonoidalMap.singleton compName successCase
  | otherwise = MonoidalMap.singleton compName $ failureCase failures
  where
    compName = T.intercalate "." $ names <> name el
    failures = map (T.pack . strContent) $ children "failure" el

matchQName :: String -> QName -> Bool
matchQName = flip $ (==) . qName

children :: String -> Element -> [Element]
children = filterChildrenName . matchQName

name :: Element -> [Text]
name = maybeToList . fmap T.pack . findAttrBy (matchQName "name")

-- * Postgresql outout

runMigrations :: Connection -> IO ()
runMigrations conn = do
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS test_suite_runs (id SERIAL PRIMARY KEY, run_id VARCHAR, time TIMESTAMP, version VARCHAR, suite VARCHAR, failed_runs INT, successful_runs INT)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS test_case_runs (id SERIAL PRIMARY KEY, name VARCHAR, test_suite_run_id integer, failure_count integer, success_count integer)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS test_case_failures (id SERIAL PRIMARY KEY, test_case_run_id integer, failure_log text)"

pushToPostgresql :: Options Unwrapped -> (MonoidalMap TestCase Report, Sum Int, Sum Int) -> IO ()
pushToPostgresql opts (reports, failedRuns, successfulRuns) = do
  let connInfo =
        ConnectInfo
          { connectHost = opts.postgresqlHost,
            connectPort = opts.postgresqlPort,
            connectUser = opts.postgresqlUser,
            connectPassword = opts.postgresqlPassword,
            connectDatabase = opts.postgresqlDatabase
          }
  currentTime <- getCurrentTime
  bracket (connect connInfo) close $ \conn -> do
    runMigrations conn
    suiteRunId <-
      extractId
        =<< returning
          conn
          "INSERT INTO test_suite_runs (run_id, time, version, suite, failed_runs, successful_runs) VALUES (?,?,?,?,?,?) RETURNING id"
          [(opts.runId, maybe currentTime zonedTimeToUTC opts.time, opts.codeVersion, opts.suiteName, getSum failedRuns, getSum successfulRuns)]
    let saveTestCaseRun testCase report = do
          testCaseRunId <-
            extractId
              =<< returning
                conn
                "INSERT INTO test_case_runs (name, test_suite_run_id, failure_count, success_count) VALUES (?,?,?,?) RETURNING id"
                [(testCase, suiteRunId, report.failure, report.success)]
          void $
            executeMany conn "INSERT INTO test_case_failures (test_case_run_id, failure_log) VALUES (?,?)" $
              map (testCaseRunId,) report.failureDesc
    void $ MonoidalMap.traverseWithKey saveTestCaseRun reports

extractId :: (HasCallStack) => [Only Int] -> IO Int
extractId [] = error $ "No ID returned by query"
extractId (Only x : _) = pure x
