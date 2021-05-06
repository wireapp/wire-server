-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Runner (testObjects) where

import Data.Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import Imports
import Test.Tasty.HUnit
import Type.Reflection (typeRep)

testObjects :: forall a . (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => [(a, FilePath)] -> IO ()
testObjects objs = do
  allFilesExist <- and <$> traverse (uncurry testObject) objs
  assertBool "Some golden JSON files do not exist" allFilesExist

testObject :: forall a . (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => a -> FilePath -> IO Bool
testObject obj path = do
  let actualValue = toJSON obj :: Value
      actualJson = encode actualValue
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

assertRight :: Show a => Either a b -> IO b
assertRight =
  \case
    Left a -> assertFailure $ "Expected Right, got Left: " <> show a
    Right b -> pure b
