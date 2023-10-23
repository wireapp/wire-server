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

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as T
import Imports hiding (Map)
import Options.Generic
import Prettyprinter
import Prettyprinter.Render.Text
import Prometheus
import Text.XML.Light

main :: IO ()
main = do
  opts :: Options Unwrapped <- unwrapRecord "Test Statistics"
  xmlFiles <- map ((opts.testResults <> "/") <>) . filter (".xml" `isSuffixOf`) <$> listDirectory opts.testResults
  reports <- mconcat <$> traverse parse xmlFiles
  pushFailureDescriptions opts.failureReport reports
  pushMetrics opts.metricName reports
  LBS.putStrLn =<< exportMetricsAsText

-- * Command Line Options

data Options w = Options
  { testResults :: w ::: FilePath <?> "Directory containing test results in the JUNIT xml format. All files with 'xml' extension in this directory will be considered test results",
    failureReport :: w ::: FilePath <?> "Path to output file containing failure formatted as markdown.",
    metricName :: w ::: Text <?> "Name of the prometheus metric" <!> "flake_news_test_case_results"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped)

-- * Intermediate representation

type TestCase = Text

data Report = Report
  { success :: Int,
    failure :: Int,
    failureDesc :: Set Text
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

failureCase :: Set Text -> Report
failureCase = Report 0 1

-- * XML parser

parse :: FilePath -> IO (MonoidalMap TestCase Report)
parse fn = parseRaw <$> T.readFile fn

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
    failures = Set.fromList . map (T.pack . strContent) $ children "failure" el

matchQName :: String -> QName -> Bool
matchQName = flip $ (==) . qName

children :: String -> Element -> [Element]
children = filterChildrenName . matchQName

name :: Element -> [Text]
name = maybeToList . fmap T.pack . findAttrBy (matchQName "name")

-- * Metrics output (Prometheus)

pushMetrics :: Text -> MonoidalMap TestCase Report -> IO ()
pushMetrics metricName reports = do
  let metric = vector ("testcase", "result") $ gauge $ Info metricName "Result of a test case after running it a few times"
  v <- register metric
  void $ MonoidalMap.traverseWithKey (push v) reports
  where
    push v caseName report = do
      when (report.success > 0) $
        withLabel v (caseName, "success") $
          flip setGauge (fromIntegral report.success)
      when (report.failure > 0) $
        withLabel v (caseName, "failure") $
          flip setGauge (fromIntegral report.failure)

-- * Failure descriptions output (MD)

pushFailureDescriptions :: FilePath -> MonoidalMap TestCase Report -> IO ()
pushFailureDescriptions outFile reports =
  withFile outFile WriteMode $ \outFileH ->
    hPutDoc outFileH . separateByLine preamble . render $ failures reports
  where
    preamble = h1 "Failure Descriptions"
    render =
      concatWith separateByLine
        . MonoidalMap.mapWithKey
          ( curry
              ( liftM2
                  separateByLine
                  (h2 . pretty . fst)
                  (concatWith separateByLine . map (verbatim . pretty) . Set.toList . failureDesc . snd)
              )
          )
    failures = MonoidalMap.filter (not . null . failureDesc)
    h1 = onSeparateLine . underlineWith "="
    h2 = onSeparateLine . underlineWith "-"
    underlineWith symbol x = align (width x (\w -> hardline <> pretty (T.replicate w symbol)))
    verbatim = onSeparateLine . enclose "```" "```" . onSeparateLine
    onSeparateLine = enclose hardline hardline
    separateByLine a b = enclose a b hardline
