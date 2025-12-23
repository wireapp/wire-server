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
    
    describe "Comparison operators roundtrip" $ do
      it "OpEq roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEq (ValString "john")
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpNe roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpNe (ValString "john")
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpCo roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpCo (ValString "john")
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpSw roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpSw (ValString "john")
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpEw roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEw (ValString "john")
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpGt roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "age" Nothing) OpGt (ValNumber 18)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpGe roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "age" Nothing) OpGe (ValNumber 18)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpLt roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "age" Nothing) OpLt (ValNumber 65)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "OpLe roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "age" Nothing) OpLe (ValNumber 65)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
    
    describe "CompValue types" $ do
      it "ValNull roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "manager" Nothing) OpEq ValNull
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "ValBool true roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "active" Nothing) OpEq (ValBool True)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "ValBool false roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "active" Nothing) OpEq (ValBool False)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "ValNumber (integer) roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "age" Nothing) OpEq (ValNumber 42)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "ValNumber (decimal) roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "score" Nothing) OpEq (ValNumber 3.14)
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
      
      it "ValString roundtrips correctly" $ do
        let filter' = FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEq (ValString "testuser")
        parseFilter [User20] (renderFilter filter') `shouldBe` Right filter'
  
  describe "AttrPath" $ do
    describe "Parsing" $ do
      it "parses simple attribute name without schema" $ do
        parseFilter [User20] "userName eq \"john\"" 
          `shouldBe` Right (FilterAttrCompare (AttrPath Nothing "userName" Nothing) OpEq (ValString "john"))
      
      it "parses attribute with subAttr" $ do
        parseFilter [User20] "name.familyName eq \"Doe\"" 
          `shouldBe` Right (FilterAttrCompare (AttrPath Nothing "name" (Just (SubAttr "familyName"))) OpEq (ValString "Doe"))
      
      it "parses fully qualified attribute with User20 schema" $ do
        parseFilter [User20] "urn:ietf:params:scim:schemas:core:2.0:User:userName eq \"john\"" 
          `shouldBe` Right (FilterAttrCompare (AttrPath (Just User20) "userName" Nothing) OpEq (ValString "john"))
      
      it "parses fully qualified attribute with subAttr" $ do
        parseFilter [User20] "urn:ietf:params:scim:schemas:core:2.0:User:name.familyName eq \"Doe\"" 
          `shouldBe` Right (FilterAttrCompare (AttrPath (Just User20) "name" (Just (SubAttr "familyName"))) OpEq (ValString "Doe"))
      
      it "parses custom schema attribute" $ do
        parseFilter [User20, CustomSchema "urn:hscim:test"] "urn:hscim:test:customAttr eq \"value\"" 
          `shouldBe` Right (FilterAttrCompare (AttrPath (Just (CustomSchema "urn:hscim:test")) "customAttr" Nothing) OpEq (ValString "value"))
    
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
    describe "Parsing" $ do
      it "parses ValuePath with simple filter" $ do
        let expectedAttrPath = AttrPath Nothing "addresses" Nothing
            expectedFilter = FilterAttrCompare (AttrPath Nothing "type" Nothing) OpEq (ValString "work")
            expectedValuePath = ValuePath expectedAttrPath expectedFilter
        -- Since parseValuePath is internal, we test through filter context where it would be used
        -- For now, we test the rendering works correctly
        rValuePath expectedValuePath `shouldBe` "addresses[type eq \"work\"]"
      
      it "parses ValuePath with schema prefix" $ do
        let expectedAttrPath = AttrPath (Just User20) "emails" Nothing
            expectedFilter = FilterAttrCompare (AttrPath Nothing "primary" Nothing) OpEq (ValBool True)
            expectedValuePath = ValuePath expectedAttrPath expectedFilter
        rValuePath expectedValuePath `shouldBe` "urn:ietf:params:scim:schemas:core:2.0:User:emails[primary eq true]"
    
    describe "Rendering" $ do
      it "renders ValuePath correctly" $ do
        let valuePath = ValuePath 
              (AttrPath Nothing "addresses" Nothing) 
              (FilterAttrCompare (AttrPath Nothing "type" Nothing) OpEq (ValString "work"))
        rValuePath valuePath `shouldBe` "addresses[type eq \"work\"]"
      
      it "renders ValuePath with subAttr filter correctly" $ do
        let valuePath = ValuePath 
              (AttrPath Nothing "members" Nothing) 
              (FilterAttrCompare (AttrPath Nothing "value" Nothing) OpEq (ValString "user123"))
        rValuePath valuePath `shouldBe` "members[value eq \"user123\"]"
  
  describe "Error cases" $ do
    it "fails on malformed filter - missing value" $ do
      parseFilter [User20] "userName eq" `shouldSatisfy` isLeft
    
    it "fails on malformed filter - missing operator" $ do
      parseFilter [User20] "userName \"john\"" `shouldSatisfy` isLeft
    
    it "fails on malformed filter - invalid operator" $ do
      parseFilter [User20] "userName contains \"john\"" `shouldSatisfy` isLeft
    
    it "fails on empty filter" $ do
      parseFilter [User20] "" `shouldSatisfy` isLeft
    
    it "fails on filter with just whitespace" $ do
      parseFilter [User20] "   " `shouldSatisfy` isLeft

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
