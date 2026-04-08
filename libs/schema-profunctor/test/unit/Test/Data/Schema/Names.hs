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
      mempty
      "UserId_Mzg3ODM1MzE3"
      (mkSchemaName @UserId)

testSimpleTypeFromStdLib :: TestTree
testSimpleTypeFromStdLib =
  testCase "Simple type from standard library" $
    assertEqual
      mempty
      "Int_Mjg5NjU2NjEw"
      (mkSchemaName @Int)

testParameterizedTypeOne :: TestTree
testParameterizedTypeOne =
  testCase "Parameterized type with one parameter" $ do
    assertEqual
      mempty
      "Maybe_Int_LTg4NDMwMDQ1"
      (mkSchemaName @(Maybe Int))
    assertEqual
      mempty
      "Qualified_UserId_NjA2MzcwNjQ2"
      (mkSchemaName @(Qualified UserId))

testParameterizedTypeTwo :: TestTree
testParameterizedTypeTwo =
  testCase "Parameterized type with two parameters" $
    assertEqual
      mempty
      "Either_Int_UserId_OTAzNzE0MzA4"
      (mkSchemaName @(Either Int UserId))

testNestedParameterizedType :: TestTree
testNestedParameterizedType =
  testCase "Nested parameterized types" $ do
    assertEqual
      mempty
      "Maybe_Qualified_UserId_NjE1MjgxNDQz"
      (mkSchemaName @(Maybe (Qualified UserId)))
    assertEqual
      mempty
      "Qualified_Maybe_Int_LTU0NDY2MjU1"
      (mkSchemaName @(Qualified (Maybe Int)))

testTupleType :: TestTree
testTupleType =
  testCase "Tuple types" $
    assertEqual
      mempty
      "Int_UserId_LTgwNjYzNzA2"
      (mkSchemaName @(Int, UserId))

testListType :: TestTree
testListType =
  testCase "List type" $
    assertEqual
      mempty
      "UserId_LTc0Mjg2NTY2"
      (mkSchemaName @[UserId])
