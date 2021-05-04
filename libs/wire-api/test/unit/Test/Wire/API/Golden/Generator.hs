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

module Test.Wire.API.Golden.Generator where

import Imports
import Test.Tasty.QuickCheck (Arbitrary (..), generate)
import Type.Reflection (typeRep)
import Wire.API.User (UserProfile)
import Wire.API.Conversation (Conversation)

-- NOTE: this will generate broken haskell code
--
-- To make the generated code compile, this needs to be run with
-- patched Show instances for certain types. Furthermore, a
-- substitution must be run on the generated code. This is done
-- automatically by the gentests.sh script.
generateBindings :: forall a.(Arbitrary a, Show a, Typeable a) => IORef [(String, FilePath)] -> IO ()
generateBindings mapRef = do
  objects <- replicateM 2 (generate arbitrary)
  let typeName = show (typeRep @a)
  let varName n = "testObject_" <> typeName <> show n
      fileName n =  varName n <> ".json"
      numberedObjs :: [(Int, a)] = zip [1..] objects
      generateBinding n o = do
        putStrLn $ varName n <> " :: " <> typeName
        putStrLn $ varName n <> " = " <> show o
  traverse_ (uncurry generateBinding) numberedObjs
  modifyIORef mapRef (<> map (\(n, _) -> (varName n, fileName n)) numberedObjs)

generateTestCall :: Int -> (String, FilePath) -> IO ()
generateTestCall index (var, path) = putStrLn $
  "  " <> (if index == 0 then " " else ",") <> " testObject " <> var <> " " <> show path

generateTestModule :: IO ()
generateTestModule = do
  mapRef <- newIORef mempty
  generateBindings @UserProfile mapRef
  generateBindings @Conversation mapRef
  theMap <- readIORef mapRef
  putStrLn "tests :: TestTree"
  putStrLn "tests = testGroup \"Golden tests\" $ ["
  traverse_ (uncurry generateTestCall) (zip [0 :: Int ..] theMap)
  putStrLn "  ]"
