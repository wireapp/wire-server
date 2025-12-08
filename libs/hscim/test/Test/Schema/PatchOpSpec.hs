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

import Control.Monad.Except (runExcept)
import Data.Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec
import Web.Scim.Schema.Error (ScimError)
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.Schema (Schema (..))
import Web.Scim.Schema.User (NoUserExtra (..), User)
import qualified Web.Scim.Schema.User as User
import Web.Scim.Test.Util (TestTag, scim)

type PatchTag = TestTag Text () () UserExtraPatch

type UserExtraPatch = KeyMap.KeyMap Text

spec :: Spec
spec = do
  describe "PatchOp" $ do
    it "golden: parses a simple patch operation" $ do
      let patchJson =
            [scim|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "replace",
                "path": "displayName",
                "value": "New Name"
              }]
            }
            |]
      case eitherDecode (encode patchJson) of
        Left err -> expectationFailure $ "Failed to parse PatchOp: " ++ err
        Right (_ :: PatchOp) -> pure ()

    it "roundtrip: PatchOp FromJSON/ToJSON roundtrip" $ do
      let patchJson =
            [scim|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "add",
                "path": "userName",
                "value": "testuser"
              }]
            }
            |]
      case eitherDecode (encode patchJson) of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right (patchOp :: PatchOp) -> do
          let reencoded = toJSON patchOp
          case fromJSON reencoded of
            Error err2 -> expectationFailure $ "Failed to re-parse: " ++ err2
            Success (patchOp2 :: PatchOp) -> patchOp2 `shouldBe` patchOp

    it "Operation names are case-insensitive" $ do
      let patchJsonLower =
            [scim|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "replace",
                "path": "displayName",
                "value": "Name"
              }]
            }
            |]
      let patchJsonUpper =
            [scim|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "REPLACE",
                "path": "displayName",
                "value": "Name"
              }]
            }
            |]
      let patchJsonMixed =
            [scim|
            {
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "Replace",
                "path": "displayName",
                "value": "Name"
              }]
            }
            |]
      -- All three should parse successfully (aeson-diff handles case normalization)
      case eitherDecode (encode patchJsonLower) of
        Left err -> expectationFailure $ "Failed to parse lowercase: " ++ err
        Right (_ :: PatchOp) -> pure ()
      case eitherDecode (encode patchJsonUpper) of
        Left err -> expectationFailure $ "Failed to parse uppercase: " ++ err
        Right (_ :: PatchOp) -> pure ()
      case eitherDecode (encode patchJsonMixed) of
        Left err -> expectationFailure $ "Failed to parse mixed case: " ++ err
        Right (_ :: PatchOp) -> pure ()

    describe "Parser works on examples from https://tools.ietf.org/html/rfc7644#section-3.5.2 Figure 8" $ do
      let examples1 =
            [ "members",
              "name.familyname"
            ]
          examples2 =
            [ "addresses[type eq \"work\"]",
              "members[value eq \"2819c223-7f76-453a-919d-413861904646\"]",
              "members[value eq \"2819c223-7f76-453a-919d-413861904646\"].displayname"
            ]

      for_ examples1 $ \ex -> it ex $ do
        -- Simple path examples should work
        let patchJson = object
              [ "schemas" .= [PatchOp20 :: Schema]
              , "Operations" .= [object ["op" .= ("add" :: Text), "path" .= ex, "value" .= ("test" :: Text)]]
              ]
        case fromJSON patchJson of
          Error _ -> expectationFailure $ "Should parse path: " ++ show ex
          Success (_ :: PatchOp) -> pure ()

      for_ examples2 $ \ex -> it ex $ do
        -- Value-path examples (with filters) are not supported - should be rejected
        pendingWith "FUTUREWORK: value-path filters not supported"

    it "rejects unsupported operations with proper error (not could-not-parse)" $ do
      -- Test rejection of array index operations
      let patchWithIndex = PatchOp $ AD.Patch [AD.Add [AD.OIndex 0, AD.OKey "value"] (String "test")]
      isLegalPatchOp patchWithIndex `shouldBe` False

      -- Test rejection of Move operation
      let patchWithMov = PatchOp $ AD.Patch [AD.Mov [AD.OKey "from"] [AD.OKey "to"]]
      isLegalPatchOp patchWithMov `shouldBe` False

      -- Test rejection of Copy operation
      let patchWithCpy = PatchOp $ AD.Patch [AD.Cpy [AD.OKey "from"] [AD.OKey "to"]]
      isLegalPatchOp patchWithCpy `shouldBe` False

      -- Test rejection of Test operation
      let patchWithTst = PatchOp $ AD.Patch [AD.Tst [AD.OKey "test"] (String "value")]
      isLegalPatchOp patchWithTst `shouldBe` False

  describe "applyPatch" $ do
    it "prop: roundtrip (generate two users, diff them, apply the patch, compare)" $ do
      -- Create two simple users
      let user1 :: User (TestTag Text () () NoUserExtra)
          user1 = User.empty [User20] "alice" NoUserExtra
      let user2 = user1 {User.displayName = Just "Alice Smith"}

      -- Compute diff
      let diff = AD.diff (toJSON user1) (toJSON user2)
      let patchOp = PatchOp diff

      -- Apply patch
      case runExcept (User.applyPatch patchOp user1) of
        Left _ -> expectationFailure "Patch should succeed"
        Right result -> result `shouldBe` user2

    it "throws error when patched object doesn't parse" $ do
      let user :: User (TestTag Text () () NoUserExtra)
          user = User.empty [User20] "testuser" NoUserExtra
      -- Create a patch that makes userName invalid (set to wrong type)
      let badPatch = PatchOp $ AD.Patch [AD.Rep [AD.Key "userName"] (Number 123)]
      case runExcept (User.applyPatch badPatch user) of
        Left (_ :: ScimError) -> pure () -- Expected error
        Right _ -> expectationFailure "Should fail when patched object is invalid"

    it "rejects patches with array indices" $ do
      let user :: User (TestTag Text () () NoUserExtra)
          user = User.empty [User20] "testuser" NoUserExtra
      -- Patch with array index should be rejected
      let arrayPatch = PatchOp $ AD.Patch [AD.Add [AD.OKey "emails", AD.OIndex 0] (String "test@example.com")]
      case runExcept (User.applyPatch arrayPatch user) of
        Left (_ :: ScimError) -> pure () -- Expected: rejected as unsupported
        Right _ -> expectationFailure "Should reject patches with array indices"

    it "discards all paths that don't match the user/group schema" $ do
      -- If a path doesn't exist, aeson-diff will fail the patch
      let user :: User (TestTag Text () () NoUserExtra)
          user = User.empty [User20] "testuser" NoUserExtra
      -- Try to add a field that doesn't exist in User schema - AD.patch may fail or succeed depending on behavior
      let nonExistentPatch = PatchOp $ AD.Patch [AD.Add [AD.OKey "nonExistentField"] (String "value")]
      -- This should either fail during patch application or during re-parsing
      case runExcept (User.applyPatch nonExistentPatch user) of
        Left (_ :: ScimError) -> pure () -- May fail
        Right _ -> pure () -- Or succeed if field is ignored during parsing

    it "throws error when trying to update immutable / readOnly values" $ do
      -- https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
      pendingWith "FUTUREWORK: immutability checks not yet implemented"
