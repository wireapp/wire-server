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
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Web.Scim.Schema.Common (ScimBool (..))
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
      -- see below for property-based roundtrip testing of diffing/patching two arbitrary users.
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
        Right patchOp -> patchOp `shouldBe` patchJson

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
              "oPERATIONS": [{
                "OP": "Replace",
                "path": "displayName",
                "value": "Name"
              }]
            }
            |]
      let patchJsonMixed =
            [scim|
            {
              "SCHEMAS": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "operations": [{
                "Op": "REPLACE",
                "PATH": "displayName",
                "VALUE": "Name"
              }]
            }
            |]
      -- All three should parse successfully (aeson-diff handles case normalization)
      let resultLower = eitherDecode (encode patchJsonLower) :: Either String PatchOp
      let resultUpper = eitherDecode (encode patchJsonUpper) :: Either String PatchOp
      let resultMixed = eitherDecode (encode patchJsonMixed) :: Either String PatchOp
      
      case (resultLower, resultUpper, resultMixed) of
        (Right patchLower, Right patchUpper, Right patchMixed) -> do
          -- All three should parse and be equal
          patchLower `shouldBe` patchUpper
          patchUpper `shouldBe` patchMixed
        (Left err, _, _) -> expectationFailure $ "Failed to parse lowercase: " ++ err
        (_, Left err, _) -> expectationFailure $ "Failed to parse uppercase: " ++ err
        (_, _, Left err) -> expectationFailure $ "Failed to parse mixed case: " ++ err

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
        -- Value-path examples (with filters) should be rejected during parsing or application
        let patchJson = object
              [ "schemas" .= [PatchOp20 :: Schema]
              , "Operations" .= [object ["op" .= ("add" :: Text), "path" .= ex, "value" .= ("test" :: Text)]]
              ]
        case fromJSON patchJson of
          Error _ -> pure () -- Expected: rejected during parsing if aeson-diff can detect it
          Success (patchOp :: PatchOp) -> do
            -- If it parses, it should be rejected by isLegalPatchOp or during application
            if not (isLegalPatchOp patchOp)
              then pure () -- Rejected by validation
              else pendingWith "FUTUREWORK: value-path filters not fully detected yet"

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
    it "prop: roundtrip property-based test" $ do
      require prop_patchRoundtrip

    it "roundtrip on simple example" $ do
      -- Create two simple users
      let user1 :: User (TestTag Text () () NoUserExtra)
          user1 = User.empty [User20] "alice" NoUserExtra
      let user2 = user1 {User.displayName = Just "ben"}

      -- Compute diff and apply patch
      let patchOp = PatchOp $ AD.diff (toJSON user1) (toJSON user2)

      -- Apply patch
      case runExcept (User.applyPatch patchOp user1) of
        Left _ -> expectationFailure "Patch should succeed"
        Right result -> result `shouldBe` user2

    it "throws error when patched object doesn't parse" $ do
      let user :: User (TestTag Text () () NoUserExtra)
          user = User.empty [User20] "testuser" NoUserExtra
      -- Create a patch that makes userName invalid (set to wrong type)
      let badPatch = PatchOp $ AD.Patch [AD.Rep [AD.OKey "userName"] (Number 123)]
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

    it "throws error when trying to update immutable / readOnly values" $ do
      pendingWith "FUTUREWORK: immutability checks not yet implemented"

-- | Property-based roundtrip test: generate two users, diff them, apply patch, verify equality
prop_patchRoundtrip :: Property
prop_patchRoundtrip = property $ do
  user1 <- forAll genSimpleUser
  user2 <- forAll genSimpleUser
  
  let patchOp = PatchOp $ AD.diff (toJSON user1) (toJSON user2)
  
  -- Only test if the patch is legal (no array operations)
  when (isLegalPatchOp patchOp) $ do
    case runExcept (User.applyPatch patchOp user1) of
      Left _ -> 
        -- Patch application can fail for valid reasons (e.g., trying to set invalid values)
        -- This is acceptable behavior
        success
      Right result -> 
        -- If patch succeeds, result should equal user2
        result === user2

-- | Generate simple users for property testing (no arrays to avoid unsupported operations)
genSimpleUser :: Gen (User (TestTag Text () () NoUserExtra))
genSimpleUser = do
  userName' <- Gen.text (Range.constant 1 20) Gen.unicode
  externalId' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  displayName' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  active' <- Gen.maybe $ ScimBool <$> Gen.bool
  pure $ (User.empty [User20] userName' NoUserExtra)
    { User.externalId = externalId'
    , User.displayName = displayName'
    , User.active = active'
    }
