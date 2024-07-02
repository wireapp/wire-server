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

module Test.Wire.API.Federation.Golden.Runner
  ( testObjects,
    testFromJSONFailure,
    testFromJSONObjects,
  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Imports
import Test.HUnit
import Test.Hspec
import Type.Reflection (typeRep)

testObjects :: forall a. (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => [(a, FilePath)] -> SpecWith ()
testObjects objs = do
  for_ objs $ \(obj, filepath) ->
    specify filepath $ do
      fileExists <- testObject obj filepath
      assertBool ("Golden JSON file '" <> filepath <> "' does not exist") fileExists

testObject :: forall a. (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => a -> FilePath -> IO Bool
testObject obj path = do
  let actualValue = toJSON obj :: Value
      actualJson = encodePretty actualValue
      dir = "test/golden"
      fullPath = dir <> "/" <> path
  createDirectoryIfMissing True dir
  exists <- doesFileExist fullPath
  unless exists $ ByteString.writeFile fullPath (LBS.toStrict actualJson)

  expectedValue <- assertRight =<< eitherDecodeFileStrict fullPath
  assertEqual
    (show (typeRep @a) <> ": ToJSON should match golden file: " <> path)
    expectedValue
    actualValue
  assertEqual
    (show (typeRep @a) <> ": FromJSON of " <> path <> " should match object")
    (Success obj)
    (fromJSON actualValue)

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
testFromJSONFailure path = do
  let dir = "test/golden/fromJSON"
      fullPath = dir <> "/" <> path
  parsed <- eitherDecodeFileStrict @a fullPath
  case parsed of
    Right x -> assertFailure $ show (typeRep @a) <> ": FromJSON of " <> path <> ": expected failure, got " <> show x
    Left _ -> pure ()

assertRight :: (Show a) => Either a b -> IO b
assertRight =
  \case
    Left a -> assertFailure $ "Expected Right, got Left: " <> show a
    Right b -> pure b
