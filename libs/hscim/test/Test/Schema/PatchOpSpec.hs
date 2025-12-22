{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

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

module Test.Schema.PatchOpSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Result (Error, Success), Value (String), fromJSON, toJSON)
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (Gen, Property, forAll, property, tripping)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.FilterSpec (genAttrPath, genSubAttr, genValuePath)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, xit)
import Test.Schema.Util (mk_prop_caseInsensitive)
import Web.Scim.AttrName (AttrName (..))
import Web.Scim.Filter (AttrPath (..), CompValue (ValNull), CompareOp (OpEq), Filter (..), ValuePath (..))
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.Schema (Schema (User20))
import Web.Scim.Schema.User (UserTypes)
import Web.Scim.Schema.UserTypes (supportedSchemas)
import Web.Scim.Test.Util (TestTag, scim)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess (Error _) = False

genPatchOp :: forall tag. (UserTypes tag) => Gen Value -> Gen (PatchOp tag)
genPatchOp genValue = PatchOp <$> Gen.list (Range.constant 0 20) ((genOperation @tag) genValue)

genSimplePatchOp :: forall tag. (UserTypes tag) => Gen (PatchOp tag)
genSimplePatchOp = genPatchOp @tag (String <$> Gen.text (Range.constant 0 20) Gen.unicode)

genOperation :: forall tag. (UserTypes tag) => Gen Value -> Gen Operation
genOperation genValue = Operation <$> Gen.enumBounded <*> Gen.maybe (genPath @tag) <*> Gen.maybe genValue

genPath :: forall tag. (UserTypes tag) => Gen Path
genPath =
  Gen.choice
    [ IntoValuePath <$> (genValuePath @tag) <*> Gen.maybe genSubAttr,
      NormalPath <$> (genAttrPath @tag)
    ]

prop_roundtrip :: forall tag. (UserTypes tag) => Property
prop_roundtrip = property $ do
  x <- forAll $ genPath @tag
  tripping x (encodeUtf8 . rPath) (parseOnly $ pPath (supportedSchemas @tag))

prop_roundtrip_PatchOp :: forall tag. (UserTypes tag) => Property
prop_roundtrip_PatchOp = property $ do
  -- Just some strings for now. However, should be constrained to what the
  -- PatchOp is operating on in the future... We need better typed PatchOp for
  -- this. TODO(arianvp)
  x <- forAll (genSimplePatchOp @tag)
  tripping x toJSON fromJSON

type PatchTestTag = TestTag () () () ()

spec :: Spec
spec = do
  describe "Patchable" $
    describe "HashMap Text Text" $ do
      it "supports `Add` operation" $ do
        let theMap = KeyMap.empty @Text
            operation = Operation Add (Just $ NormalPath (AttrPath Nothing (AttrName "key") Nothing)) $ Just "value"
        applyOperation theMap operation `shouldBe` Right (KeyMap.singleton "key" "value")
      it "supports `Replace` operation" $ do
        let theMap = KeyMap.singleton @Text "key" "value1"
            operation = Operation Replace (Just $ NormalPath (AttrPath Nothing (AttrName "key") Nothing)) $ Just "value2"
        applyOperation theMap operation `shouldBe` Right (KeyMap.singleton "key" "value2")
      it "supports `Delete` operation" $ do
        let theMap = KeyMap.fromList @Text [("key1", "value1"), ("key2", "value2")]
            operation = Operation Remove (Just $ NormalPath (AttrPath Nothing (AttrName "key1") Nothing)) Nothing
        applyOperation theMap operation `shouldBe` Right (KeyMap.singleton "key2" "value2")
      it "gracefully rejects invalid/unsupported operations" $ do
        let theMap = KeyMap.fromList @Text [("key1", "value1"), ("key2", "value2")]
            key1Path = AttrPath Nothing (AttrName "key1") Nothing
            key2Path = AttrPath Nothing (AttrName "key2") Nothing
            invalidOperations =
              [ Operation Add (Just $ NormalPath key1Path) Nothing, -- Nothing to add
                Operation Replace (Just $ NormalPath key1Path) Nothing, -- Nothing to replace
                Operation Add (Just $ IntoValuePath (ValuePath key1Path (FilterAttrCompare key2Path OpEq ValNull)) Nothing) Nothing
                -- IntoValuePaths don't make sense for HashMap Text Text
              ]
        mapM_ (\o -> applyOperation theMap o `shouldSatisfy` isLeft) invalidOperations
  describe "urn:ietf:params:scim:api:messages:2.0:PatchOp" $ do
    describe "The body of each request MUST contain the \"schemas\" attribute with the URI value of \"urn:ietf:params:scim:api:messages:2.0:PatchOp\"." $
      it "rejects an empty schemas list" $ do
        fromJSON @(PatchOp PatchTestTag)
          [scim| {
          "schemas": [],
          "operations": []
        }|]
          `shouldSatisfy` (not . isSuccess)
    -- TODO(arianvp): We don't support arbitrary path names (yet)
    it "roundtrips Path" $ require $ prop_roundtrip @PatchTestTag
    it "roundtrips PatchOp" $ require $ prop_roundtrip_PatchOp @PatchTestTag
    it "case-insensitive" $ require $ mk_prop_caseInsensitive (genSimplePatchOp @PatchTestTag)
    it "rejects invalid operations" $
      fromJSON @(PatchOp PatchTestTag)
        [scim| {
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "operations": [{"op":"unknown"}]
      }|]
        `shouldSatisfy` (not . isSuccess)
    -- TODO(arianvp/akshay): Implement if required
    xit "rejects unknown paths" $
      Aeson.parse (pathFromJSON [User20]) (Aeson.String "unknown.field") `shouldSatisfy` (not . isSuccess)
    it "rejects invalid paths" $
      Aeson.parse (pathFromJSON [User20]) "unknown]field" `shouldSatisfy` (not . isSuccess)
    describe "Examples from https://tools.ietf.org/html/rfc7644#section-3.5.2 Figure 8" $ do
      let examples =
            [ "members",
              "name.familyname",
              "addresses[type eq \"work\"]",
              "members[value eq \"2819c223-7f76-453a-919d-413861904646\"]",
              "members[value eq \"2819c223-7f76-453a-919d-413861904646\"].displayname"
            ]
      for_ examples $ \p -> it ("parses " ++ show p) $ rPath <$> parseOnly (pPath (supportedSchemas @PatchTestTag)) p `shouldBe` Right (decodeUtf8 p)
