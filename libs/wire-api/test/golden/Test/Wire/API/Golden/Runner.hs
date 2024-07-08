-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Runner
  ( testObjects,
    protoTestObjects,
    testFromJSONFailure,
    testFromJSONFailureWithMsg,
    testFromJSONObject,
    testFromJSONObjects,
  )
where

import Data.Aeson
import Data.Aeson.Diff qualified as AD
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty')
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Message (Message)
import Data.ProtoLens.TextFormat (readMessage, showMessage)
import Data.String.Conversions
import Data.Text.Lazy.IO qualified as LText
import Imports
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Type.Reflection (typeRep)
import Wire.API.ServantProto

testObjects :: forall a. (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => [(a, FilePath)] -> [TestTree]
testObjects = fmap (\(obj, path) -> testCase path $ testObject obj path)

testObject :: forall a. (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => a -> FilePath -> Assertion
testObject obj path = do
  let actualValue = toJSON obj :: Value
      actualJson = encodePretty' config actualValue
      dir = "test/golden"
      fullPath = dir <> "/" <> path
  createDirectoryIfMissing True dir
  exists <- doesFileExist fullPath
  unless exists $ ByteString.writeFile fullPath (LBS.toStrict actualJson)

  expectedValue :: Value <- assertRight =<< eitherDecodeFileStrict fullPath
  assertBool
    ( show (typeRep @a)
        <> ": ToJSON should match golden file: "
        <> path
        <> "\n\nexpected:\n"
        <> cs (encodePretty' config expectedValue)
        <> "\n\nactual:\n"
        <> cs (encodePretty' config actualValue)
        <> "\n\ndiff:\n"
        <> cs (encodePretty' config (AD.diff expectedValue actualValue))
    )
    (expectedValue == actualValue)
  assertEqual
    (show (typeRep @a) <> ": FromJSON of " <> path <> " should match object")
    (Success obj)
    (fromJSON actualValue)
  assertBool ("JSON golden file " <> path <> " does not exist") exists
  where
    config = defConfig {confCompare = compare, confTrailingNewline = True}

protoTestObjects ::
  forall m a.
  (Typeable a, ToProto a, FromProto a, Eq a, Show a, Show m, Eq m, Message m) =>
  [(a, FilePath)] ->
  IO ()
protoTestObjects objs = do
  allFilesExist <- and <$> traverse (uncurry (protoTestObject @m)) objs
  assertBool "Some golden protobuf files do not exist" allFilesExist

protoTestObject ::
  forall m a.
  (Typeable a, ToProto a, FromProto a, Eq a, Show a, Show m, Eq m, Message m) =>
  a ->
  FilePath ->
  IO Bool
protoTestObject obj path = do
  let actual = toProto obj
  msg <- assertRight (decodeMessage @m actual)
  let pretty = showMessage msg
      dir = "test/golden"
      fullPath = dir <> "/" <> path
  createDirectoryIfMissing True dir
  exists <- doesFileExist fullPath
  unless exists $ writeFile fullPath pretty

  msgText <- LText.readFile fullPath
  expected <- assertRight (readMessage @m msgText)
  assertEqual
    (show (typeRep @a) <> ": ToProto should match golden file: " <> path)
    expected
    msg
  assertEqual
    (show (typeRep @a) <> ": FromProto of " <> path <> " should match object")
    (Right obj)
    (fromProto (encodeMessage expected))

  pure exists

testFromJSONObjects :: forall a. (Typeable a, FromJSON a, Eq a, Show a) => [(a, FilePath)] -> IO ()
testFromJSONObjects = traverse_ (uncurry testFromJSONObject)

testFromJSONObject :: forall a. (Typeable a, FromJSON a, Eq a, Show a) => a -> FilePath -> IO ()
testFromJSONObject expected path = do
  let dir = "test/golden/fromJSON"
      fullPath = dir <> "/" <> path
  parsed <- eitherDecodeFileStrict fullPath
  assertEqual (show (typeRep @a) <> ": FromJSON of " <> path <> " should match object") (Right expected) parsed

testFromJSONFailure :: forall a. (Typeable a, FromJSON a, Show a) => FilePath -> IO ()
testFromJSONFailure = testFromJSONFailureWithMsg @a Nothing

testFromJSONFailureWithMsg :: forall a. (Typeable a, FromJSON a, Show a) => Maybe String -> FilePath -> IO ()
testFromJSONFailureWithMsg msg path = do
  let dir = "test/golden/fromJSON"
      fullPath = dir <> "/" <> path
  parsed <- eitherDecodeFileStrict @a fullPath
  case parsed of
    Right x -> assertFailure $ failurePrefix <> ": expected failure, got " <> show x
    Left err -> case msg of
      Nothing -> pure ()
      Just m ->
        assertBool
          ( failurePrefix
              <> " had a wrong failure: "
              <> show m
              <> " is not contained in "
              <> show err
          )
          (m `isSuffixOf` err)
  where
    failurePrefix = show (typeRep @a) <> ": FromJSON of " <> path

assertRight :: (Show a) => Either a b -> IO b
assertRight =
  \case
    Left a -> assertFailure $ "Expected Right, got Left: " <> show a
    Right b -> pure b
