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

import Data.Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.QQ (aesonQQ)
import Data.Either
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.User
import Web.Scim.Test.Util

type PatchTag = TestTag Text () () UserExtraPatch

type UserExtraPatch = KeyMap.KeyMap Text

spec :: Spec
spec = do
  describe "PatchOp" $ do
    it "golden + simple roundtrip" $ do
      -- we don't need a property here, one sample is enough: AD.Patch
      -- is tested in aeson-diff.
      let want :: Value =
            [aesonQQ|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "add",
                "path": "userName",
                "value": "testuser"
              }]
            }
            |]
      case eitherDecode (encode want) of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right (have :: PatchOp) -> toJSON have `shouldBe` want

    it "Operation attributes and value attributes are case-insensitive" $ do
      let patches :: [Value] =
            [ [aesonQQ|
              {
                "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                "Operations": [{
                  "op": "replace",
                  "path": "displayName",
                  "value": "Name"
                }]
              }
              |],
              [aesonQQ|
              {
                "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                "Operations": [{
                  "op": "REPLACE",
                  "path": "displayName",
                  "value": "Name"
                }]
              }
              |],
              [aesonQQ|
              {
                "Schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                "Operations": [{
                  "OP": "Replace",
                  "PATH": "dISPlayName",
                  "VALUE": "Name"
                }]
              }
              |]
            ]
      case nub $ (eitherDecode @PatchOp . encode) <$> patches of
        [Right _] -> pure ()
        bad -> expectationFailure $ "Case insensitivity check failed: " ++ show bad

    describe "Parser works on examples from https://tools.ietf.org/html/rfc7644#section-3.5.2 Figure 8" $ do
      context "working / implemented" $ do
        let examples =
              [ "members",
                "name.familyname"
              ]
        for_ examples $ \ex -> it ex $ eitherDecode @Key (fromString ex) `shouldSatisfy` isRight

      context "FUTUREWORK" $ do
        let examples =
              [ "addresses[type eq \"work\"]",
                "members[value eq \"2819c223-7f76-453a-919d-413861904646\"]",
                "members[value eq \"2819c223-7f76-453a-919d-413861904646\"].displayname"
              ]
        for_ examples $ \ex -> it ex $ eitherDecode @Key (fromString ex) `shouldSatisfy` isLeft

  describe "applyPatch" $ do
    prop "roundtrip (generate two users/groups, diff them, apply the patch, compare)" $
      \(barbie :: User (TestTag Text () () NoUserExtra)) changedWant -> 
        let patchOp = PatchOp (AD.diff (toJSON barbie) (toJSON changedWant))
            changedHave = applyPatch patchOp barbie
         in changedHave === Right changedWant 

    it "throws error when patched object doesn't parse" $ do
      _ <- todo
      True `shouldBe` False

    it "discards all paths that don't match the user/group schema" $ do
      _ <- todo
      True `shouldBe` False

    it "throws error when trying to update immutable / readOnly values" $ do
      -- https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
      _ <- todo
      True `shouldBe` False

instance Arbitrary (User (TestTag Text () () NoUserExtra)) where
-- TODO: move this to test module in library.
  arbitrary = undefined
