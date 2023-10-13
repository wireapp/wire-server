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

import Data.List
import Data.Map qualified as Map
import Imports
import Text.XML.Light

main :: IO ()
main = liftM2 (>>) pushMetrics pushFailureDescriptions . merge =<< mapM parse =<< getArgs

-- * Intermediate representation

type TestCase = String

data Report = Report
  { success :: Int,
    failure :: Int,
    failureDesc :: [String]
  }
  deriving (Show)

instance Semigroup Report where
  a <> b =
    Report
      (a.success + b.success)
      (a.failure + b.failure)
      (a.failureDesc <> b.failureDesc)

successCase :: Report
successCase = Report 1 0 []

failureCase :: [String] -> Report
failureCase = Report 0 1

-- * XML parser

parse :: FilePath -> IO (Map TestCase Report)
parse fn = parseRaw <$> readFile fn

parseRaw :: String -> Map TestCase Report
parseRaw = merge . map parseTestSuites . selectTestSuites . parseXML
  where
    selectTestSuites = filter (matchQName "testsuites" . elName) . onlyElems

parseTestSuites :: Element -> Map TestCase Report
parseTestSuites = merge . liftM2 map (parseTestSuite . name) (children "testsuite")

parseTestSuite :: [String] -> Element -> Map TestCase Report
parseTestSuite names =
  liftM2
    (Map.unionWith (<>))
    (merge . liftM2 map (parseTestSuite . (names <>) . name) (children "testsuite"))
    (merge . liftM2 map (parseTestCase . (names <>) . name) (children "testcase"))

parseTestCase :: [String] -> Element -> Map TestCase Report
parseTestCase names el
  | null failures = Map.singleton compName successCase
  | otherwise = Map.singleton compName $ failureCase failures
  where
    compName = intercalate "." $ names <> name el
    failures = map strContent $ children "failure" el

merge :: [Map TestCase Report] -> Map TestCase Report
merge = Map.unionsWith (<>)

matchQName :: String -> QName -> Bool
matchQName = flip $ (==) . qName

children :: String -> Element -> [Element]
children = filterChildrenName . matchQName

name :: Element -> [String]
name = maybeToList . findAttrBy (matchQName "name")

-- * Metrics output

pushMetrics :: Map TestCase Report -> IO ()
pushMetrics = undefined

-- * Failure descriptions output

pushFailureDescriptions :: Map TestCase Report -> IO ()
pushFailureDescriptions = undefined
