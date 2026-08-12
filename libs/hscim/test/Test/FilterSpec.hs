{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Aeson
import qualified Data.Attoparsec.ByteString as Atto
import Data.Text (cons)
import Data.Text.Encoding (encodeUtf8)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Imports
import Test.Hspec
import Web.Scim.AttrName
import Web.Scim.Filter
import Web.Scim.Schema.Schema (Schema (..))
import Web.Scim.Schema.User (NoUserExtra)
import Web.Scim.Schema.UserTypes
import Web.Scim.Test.Util (TestTag)

spec :: Spec
spec = pure ()

{-
prop_roundtrip :: forall tag. (UserTypes tag) => Property
prop_roundtrip = property $ do
  x <- forAll $ genFilter @tag
  tripping x renderFilter $ parseFilter (supportedSchemas @tag)

spec :: Spec
spec = do
  describe "AttrPath" $ do
    describe "golden" $ do
      let examples :: [(String, Either String AttrPath)]
          examples =
            [ ( "members",
                Right $ AttrPath Nothing (AttrName "members") Nothing
              ),
              ( "name.familyname",
                Right $ AttrPath Nothing (AttrName "name") (Just (SubAttr (AttrName "familyname")))
              ),
              ( "",
                Left "Error in $: letter_ascii: not enough input" -- FUTUREWORK: better error
              ),
              ( ".members",
                Left "Error in $: letter_ascii: Failed reading: satisfyWith" -- FUTUREWORK: better error
              ),
              ( "urn:ietf:params:scim:schemas:core:2.0:Group:members",
                -- FUTUREWORK: this must be `Right $ AttrPath (Just
                -- Group20) (AttrName "members") Nothing` (or, more
                -- likely, be rejected due to schema not being passed
                -- to parser)
                Right $ AttrPath Nothing (AttrName "urn") Nothing
              ),
              ( "urn:ietf:params:scim:schemas:core:2.0:Group:nosuchfield",
                -- FUTUREWORK: this must be `Left "..."`
                Right $ AttrPath Nothing (AttrName "urn") Nothing
              )
            ]

          runGolden :: forall a. (HasCallStack, Eq a, Show a, FromJSON a) => (String, Either String a) -> Spec
          runGolden (ex, want) = it ("attribute path: " <> show ex) $ eitherDecode @a (encode ex) `shouldBe` want

      for_ examples runGolden

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
    describe "golden" $ do
      it "renders ValuePath correctly" $ do
        let valuePath =
              ValuePath
                (AttrPath Nothing "addresses" Nothing)
                (FilterAttrCompare (AttrPath Nothing "type" Nothing) OpEq (ValString "work"))
        rValuePath valuePath `shouldBe` "addresses[type eq \"work\"]"

      it "renders ValuePath with schema prefix" $ do
        let valuePath =
              ValuePath
                (AttrPath (Just User20) "emails" Nothing)
                (FilterAttrCompare (AttrPath Nothing "primary" Nothing) OpEq (ValBool True))
        rValuePath valuePath `shouldBe` "urn:ietf:params:scim:schemas:core:2.0:User:emails[primary eq true]"

      it "renders ValuePath with subAttr filter correctly" $ do
        let valuePath =
              ValuePath
                (AttrPath Nothing "members" Nothing)
                (FilterAttrCompare (AttrPath Nothing "value" Nothing) OpEq (ValString "user123"))
        rValuePath valuePath `shouldBe` "members[value eq \"user123\"]"

      -- FUTUREWORK
      let examples :: [(Text, Either String ValuePath)]
          examples =
            [ ( "addresses[type eq \"work\"]",
                Right $
                  ValuePath
                    (AttrPath Nothing (AttrName "addresses") Nothing)
                    (mkFilter "type" OpEq "work")
              ),
              ( "members[value eq \"2819c223-7f76-453a-919d-413861904646\"]",
                Right $
                  ValuePath
                    (AttrPath Nothing (AttrName "members") Nothing)
                    (mkFilter "value" OpEq "2819c223-7f76-453a-919d-413861904646")
              )
              {- FUTUREWORK: these tests fail (not implemented)

              ( "members[type eq \"work\"].displayname",
                Right $
                  ValuePath
                    (AttrPath Nothing (AttrName "members") (Just (SubAttr (AttrName "displayname"))))
                    (mkFilter "type" OpEq "work")
              ),
              ( "members[type le \"work\" and value eq \"\"]",
                Right $
                  ValuePath
                    (AttrPath Nothing (AttrName "members") Nothing)
                    (mkFilter "type" OpLe "work")
              )
              -}
            ]

          mkFilter :: Text -> CompareOp -> Text -> Filter
          mkFilter field co val = FilterAttrCompare (AttrPath Nothing (AttrName field) Nothing) co (ValString val)

      for_ examples $ \(str, want) ->
        it ("value path: " <> show str) $
          Atto.parseOnly (pValuePath [User20]) (encodeUtf8 str) `shouldBe` want

  describe "Filter" $ do
    it "parse . render === id" $ require $ prop_roundtrip @(TestTag Text () () NoUserExtra)

    describe "golden" $ do
      it "1" $ do
        parseFilter [] ""
          `shouldBe` Left "letter_ascii: not enough input" -- FUTUREWORK: better error
        parseFilter [User20] ""
          `shouldBe` Left "letter_ascii: not enough input" -- FUTUREWORK: better error
      it "2" $ do
        parseFilter [] "nosuchfield co \"yessuchfield\""
          `shouldBe` Right (FilterAttrCompare (AttrPath Nothing "nosuchfield" Nothing) OpCo (ValString "yessuchfield"))
      it "3" $ do
        parseFilter [] ".nosuchfield eq "
          `shouldBe` Left "letter_ascii: Failed reading: satisfyWith" -- FUTUREWORK: better error
      it "4" $ do
        parseFilter [] "attr.subAttr eq \"stuff\""
          `shouldBe` Right
            ( FilterAttrCompare
                (AttrPath Nothing (AttrName "attr") (Just (SubAttr (AttrName "subAttr"))))
                OpEq
                (ValString "stuff")
            )

    describe "Comparison operators and CompValue types" $ do
      let filterExamples :: [(Text, Either Text Filter)]
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
                Left "space: not enough input" -- FUTUREWORK: better error
              ),
              ( "userName \"john\"",
                Left "Failed reading: empty" -- FUTUREWORK: better error
              ),
              ( "",
                Left "letter_ascii: not enough input" -- FUTUREWORK: better error
              ),
              ( "   ",
                Left "letter_ascii: not enough input" -- FUTUREWORK: better error
              )
            ]

      for_ filterExamples $ \(filterStr, want) ->
        it ("filter: " <> show filterStr) $
          parseFilter [User20] filterStr `shouldBe` want

    describe "AttrPath inside Filter" $ do
      describe "Parsing and rendering" $
        do
          let attrPathExamples :: [(Text, Either Text Filter)]
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
                  {- FUTUREWORK: this fails

                  -- Custom schema
                  ( "urn:hscim:test:customAttr eq \"value\"",
                    Right $ FilterAttrCompare (AttrPath (Just (CustomSchema "urn:hscim:test")) "customAttr" Nothing) OpEq (ValString "value")
                  ),
                  -}
                  -- Error case - unsupported schema (Group20 not in supported schemas for User20-only list)
                  ( "urn:ietf:params:scim:schemas:core:2.0:Group:displayName eq \"Admins\"",
                    Left "space: Failed reading: satisfyWith" -- FUTUREWORK: better error
                  ),
                  ( "",
                    Left "letter_ascii: not enough input" -- FUTUREWORK: better error
                  ),
                  ( ".userName eq \"test\"",
                    Left "letter_ascii: Failed reading: satisfyWith" -- FUTUREWORK: better error
                  )
                ]

          for_ attrPathExamples $ \(str, want) ->
            it ("filter: " <> show str) $
              parseFilter [User20] str `shouldBe` want

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
-}
