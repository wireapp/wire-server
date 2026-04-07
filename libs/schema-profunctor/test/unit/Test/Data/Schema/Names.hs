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

module Test.Data.Schema.Names where

import Data.Schema hiding (getName)
import Imports
import Test.Tasty
import Test.Tasty.HUnit

newtype UserId = UserId Text
  deriving (Eq, Show)

newtype Qualified a = Qualified a
  deriving (Eq, Show)

testSchemaNames :: TestTree
testSchemaNames =
  testGroup
    "mkSchemaName"
    [ testSimpleType,
      testSimpleTypeFromStdLib,
      testParameterizedTypeOne,
      testParameterizedTypeTwo,
      testNestedParameterizedType,
      testTupleType,
      testListType
    ]

testSimpleType :: TestTree
testSimpleType =
  testCase "Simple type from current module" $
    assertEqual
      "Should be fully qualified with module name"
      "UserId (Test.Data.Schema.Names.UserId)"
      (mkSchemaName @UserId)

testSimpleTypeFromStdLib :: TestTree
testSimpleTypeFromStdLib =
  testCase "Simple type from standard library" $
    assertEqual
      "Should be fully qualified"
      "Int (GHC.Types.Int)"
      (mkSchemaName @Int)

testParameterizedTypeOne :: TestTree
testParameterizedTypeOne =
  testCase "Parameterized type with one parameter" $ do
    assertEqual
      "Maybe Int should include both type and parameter"
      "Maybe Int (GHC.Internal.Maybe.Maybe GHC.Types.Int)"
      (mkSchemaName @(Maybe Int))
    assertEqual
      "Qualified UserId should include both type and parameter"
      "Qualified UserId (Test.Data.Schema.Names.Qualified Test.Data.Schema.Names.UserId)"
      (mkSchemaName @(Qualified UserId))

testParameterizedTypeTwo :: TestTree
testParameterizedTypeTwo =
  testCase "Parameterized type with two parameters" $
    assertEqual
      "Either should include all type parameters"
      "Either Int UserId (GHC.Internal.Data.Either.Either GHC.Types.Int Test.Data.Schema.Names.UserId)"
      (mkSchemaName @(Either Int UserId))

testNestedParameterizedType :: TestTree
testNestedParameterizedType =
  testCase "Nested parameterized types" $ do
    assertEqual
      "Maybe (Qualified UserId) should be fully expanded"
      "Maybe (Qualified UserId) (GHC.Internal.Maybe.Maybe Test.Data.Schema.Names.Qualified Test.Data.Schema.Names.UserId)"
      (mkSchemaName @(Maybe (Qualified UserId)))
    assertEqual
      "Qualified (Maybe Int) should be fully expanded"
      "Qualified (Maybe Int) (Test.Data.Schema.Names.Qualified GHC.Internal.Maybe.Maybe GHC.Types.Int)"
      (mkSchemaName @(Qualified (Maybe Int)))

testTupleType :: TestTree
testTupleType =
  testCase "Tuple types" $
    assertEqual
      "Tuple should include all element types"
      "(Int,UserId) (GHC.Tuple.Tuple2 GHC.Types.Int Test.Data.Schema.Names.UserId)"
      (mkSchemaName @(Int, UserId))

testListType :: TestTree
testListType =
  testCase "List type" $
    assertEqual
      "List should include element type"
      "[UserId] (GHC.Types.List Test.Data.Schema.Names.UserId)"
      (mkSchemaName @[UserId])
