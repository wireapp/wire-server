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

import Data.Either (isLeft)
import Data.Foldable (for_)
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
    
    describe "Comparison operators and CompValue types" $ do
      let filterExamples :: [(String, Either String Filter)]
          filterExamples =
            [ -- OpEq tests
              ( "userName eq \"john\"",
                Right $ FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEq (ValString "john")
              ),
              ( "age eq 42",
                Right $ FilterAttrCompare (AttrPath Nothing "age" Nothing) OpEq (ValNumber 42)
              ),
              ( "active eq true",
                Right $ FilterAttrCompare (AttrPath Nothing "active" Nothing) OpEq (ValBool True)
              ),
              ( "active eq false",
                Right $ FilterAttrCompare (AttrPath Nothing "active" Nothing) OpEq (ValBool False)
              ),
              ( "manager eq null",
                Right $ FilterAttrCompare (AttrPath Nothing "manager" Nothing) OpEq ValNull
              ),
              -- OpNe tests
              ( "userName ne \"john\"",
                Right $ FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpNe (ValString "john")
              ),
              -- OpCo (contains) test
              ( "userName co \"john\"",
                Right $ FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpCo (ValString "john")
              ),
              -- OpSw (starts with) test
              ( "userName sw \"john\"",
                Right $ FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpSw (ValString "john")
              ),
              -- OpEw (ends with) test
              ( "userName ew \"john\"",
                Right $ FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEw (ValString "john")
              ),
              -- OpGt test
              ( "age gt 18",
                Right $ FilterAttrCompare (AttrPath Nothing "age" Nothing) OpGt (ValNumber 18)
              ),
              -- OpGe test
              ( "age ge 18",
                Right $ FilterAttrCompare (AttrPath Nothing "age" Nothing) OpGe (ValNumber 18)
              ),
              -- OpLt test
              ( "age lt 65",
                Right $ FilterAttrCompare (AttrPath Nothing "age" Nothing) OpLt (ValNumber 65)
              ),
              -- OpLe test
              ( "age le 65",
                Right $ FilterAttrCompare (AttrPath Nothing "age" Nothing) OpLe (ValNumber 65)
              ),
              -- Decimal number
              ( "score eq 3.14",
                Right $ FilterAttrCompare (AttrPath Nothing "score" Nothing) OpEq (ValNumber 3.14)
              ),
              -- Error cases
              ( "userName eq",
                Left "Error in $: not enough input"  -- missing value
              ),
              ( "userName \"john\"",
                Left "Error in $: Failed reading: satisfyWith"  -- missing operator
              ),
              ( "",
                Left "Error in $: not enough input"  -- empty filter
              ),
              ( "   ",
                Left "Error in $: not enough input"  -- whitespace only
              )
            ]
      
      for_ filterExamples $ \(filterStr, expected) ->
        it ("filter: " <> show filterStr) $
          parseFilter [User20] filterStr `shouldBe` expected
  
  describe "AttrPath" $ do
    describe "Parsing and rendering" $ do
      let attrPathExamples :: [(String, Either String Filter)]
          attrPathExamples =
            [ -- Simple attribute without schema
              ( "userName eq \"john\"",
                Right $ FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEq (ValString "john")
              ),
              -- Attribute with subAttr
              ( "name.familyName eq \"Doe\"",
                Right $ FilterAttrCompare (AttrPath Nothing "name" (Just (SubAttr "familyName"))) OpEq (ValString "Doe")
              ),
              -- Fully qualified with User20 schema
              ( "urn:ietf:params:scim:schemas:core:2.0:User:userName eq \"john\"",
                Right $ FilterAttrCompare (AttrPath (Just User20) "userName" Nothing) OpEq (ValString "john")
              ),
              -- Fully qualified with schema and subAttr
              ( "urn:ietf:params:scim:schemas:core:2.0:User:name.familyName eq \"Doe\"",
                Right $ FilterAttrCompare (AttrPath (Just User20) "name" (Just (SubAttr "familyName"))) OpEq (ValString "Doe")
              ),
              -- Custom schema
              ( "urn:hscim:test:customAttr eq \"value\"",
                Right $ FilterAttrCompare (AttrPath (Just (CustomSchema "urn:hscim:test")) "customAttr" Nothing) OpEq (ValString "value")
              ),
              -- Error case - unsupported schema (Group20 not in supported schemas for User20-only list)
              ( "urn:ietf:params:scim:schemas:core:2.0:Group:displayName eq \"Admins\"",
                Left "Error in $: Failed reading: satisfyWith"
              ),
              -- FUTUREWORK: better error messages for invalid paths
              ( "",
                Left "Error in $: not enough input"
              ),
              ( ".userName eq \"test\"",
                Left "Error in $: Failed reading: satisfyWith"  -- FUTUREWORK: better error
              )
            ]
      
      for_ attrPathExamples $ \(filterStr, expected) ->
        it ("attribute path: " <> show filterStr) $
          parseFilter [User20] filterStr `shouldBe` expected
    
    describe "Rendering" $ do
      it "renders simple attribute without schema" $ do
        rAttrPath (AttrPath Nothing "userName" Nothing) `shouldBe` "userName"
      
      it "renders attribute with subAttr" $ do
        rAttrPath (AttrPath Nothing "name" (Just (SubAttr "familyName"))) `shouldBe` "name.familyName"
      
      it "renders fully qualified attribute with schema" $ do
        rAttrPath (AttrPath (Just User20) "userName" Nothing) 
          `shouldBe` "urn:ietf:params:scim:schemas:core:2.0:User:userName"
      
      it "renders fully qualified attribute with schema and subAttr" $ do
        rAttrPath (AttrPath (Just User20) "name" (Just (SubAttr "familyName"))) 
          `shouldBe` "urn:ietf:params:scim:schemas:core:2.0:User:name.familyName"
    
    describe "Multiple schemas" $ do
      it "can parse filter with Group20 schema when supported" $ do
        parseFilter [User20, Group20] "urn:ietf:params:scim:schemas:core:2.0:Group:displayName eq \"Admins\"" 
          `shouldBe` Right (FilterAttrCompare (AttrPath (Just Group20) "displayName" Nothing) OpEq (ValString "Admins"))
      
      it "fails to parse unsupported schema" $ do
        parseFilter [User20] "urn:ietf:params:scim:schemas:core:2.0:Group:displayName eq \"Admins\"" 
          `shouldSatisfy` isLeft
  
  describe "ValuePath" $ do
    describe "Rendering" $ do
      it "renders ValuePath correctly" $ do
        let valuePath = ValuePath 
              (AttrPath Nothing "addresses" Nothing) 
              (FilterAttrCompare (AttrPath Nothing "type" Nothing) OpEq (ValString "work"))
        rValuePath valuePath `shouldBe` "addresses[type eq \"work\"]"
      
      it "renders ValuePath with schema prefix" $ do
        let valuePath = ValuePath
              (AttrPath (Just User20) "emails" Nothing)
              (FilterAttrCompare (AttrPath Nothing "primary" Nothing) OpEq (ValBool True))
        rValuePath valuePath `shouldBe` "urn:ietf:params:scim:schemas:core:2.0:User:emails[primary eq true]"
      
      it "renders ValuePath with subAttr filter correctly" $ do
        let valuePath = ValuePath 
              (AttrPath Nothing "members" Nothing) 
              (FilterAttrCompare (AttrPath Nothing "value" Nothing) OpEq (ValString "user123"))
        rValuePath valuePath `shouldBe` "members[value eq \"user123\"]"

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
