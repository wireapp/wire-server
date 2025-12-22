{-# LANGUAGE AllowAmbiguousTypes #-}

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

module Test.FilterSpec where

import Data.Text (Text, cons)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Web.Scim.AttrName
import Web.Scim.Filter
import Web.Scim.Schema.Schema (Schema (..))
import Web.Scim.Schema.User (NoUserExtra)
import Web.Scim.Schema.UserTypes (UserTypes (supportedSchemas))
import Web.Scim.Test.Util (TestTag)

prop_roundtrip :: forall tag. (UserTypes tag) => Property
prop_roundtrip = property $ do
  x <- forAll $ genFilter @tag
  tripping x renderFilter $ parseFilter (supportedSchemas @tag)

spec :: Spec
spec = do
  describe "Filter" $ do
    it "parse . render === id" $ require $ prop_roundtrip @(TestTag Text () () NoUserExtra)

----------------------------------------------------------------------------
-- Generators

genValuePath :: forall tag. (UserTypes tag) => Gen ValuePath
genValuePath = ValuePath <$> genAttrPath @tag <*> genFilter @tag

genCompValue :: Gen CompValue
genCompValue =
  Gen.choice
    [ pure ValNull,
      ValBool <$> Gen.bool,
      ValNumber
        <$> Gen.choice
          [ Gen.realFrac_ (Range.constantFrom 0 (-100) 100),
            fromInteger <$> Gen.integral (Range.constantFrom 0 (-100) 100)
          ],
      ValString <$> Gen.text (Range.constant 0 1000) Gen.unicode
    ]

genCompareOp :: Gen CompareOp
genCompareOp = Gen.enumBounded

genSubAttr :: Gen SubAttr
genSubAttr = SubAttr <$> genAttrName

-- | FUTUREWORK: no support for custom schemas.
--
-- FUTUREWORK: we also may want to factor a bounded enum type out of the 'Schema' type for
-- this: @data Schema = Buitin BuitinSchema | Custom Text; data BuiltinSchema = ... deriving
-- (Bounded, Enum, ...)@
genSchema :: forall tag. (UserTypes tag) => Gen Schema
genSchema = Gen.element (supportedSchemas @tag)

genAttrPath :: forall tag. (UserTypes tag) => Gen AttrPath
genAttrPath = AttrPath <$> Gen.maybe (genSchema @tag) <*> genAttrName <*> Gen.maybe genSubAttr

genAttrName :: Gen AttrName
genAttrName = AttrName <$> (cons <$> Gen.alpha <*> Gen.text (Range.constant 0 50) (Gen.choice [Gen.alphaNum, Gen.constant '-', Gen.constant '_']))

genFilter :: forall tag. (UserTypes tag) => Gen Filter
genFilter =
  Gen.choice
    [ FilterAttrCompare <$> (genAttrPath @tag) <*> genCompareOp <*> genCompValue
    ]
