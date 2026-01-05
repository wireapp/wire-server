{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import qualified Data.Aeson.Pointer as AD
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types
import Data.Either
import qualified Data.List.NonEmpty as NE
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.Scim.AttrName
import Web.Scim.Filter
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.Schema
import Web.Scim.Schema.User
import Web.Scim.Test.Util

type PatchTag = TestTag Text () () UserExtraPatch

type UserExtraPatch = KeyMap.KeyMap Text

spec :: Spec
spec = do
  describe "Patch" $ do
    it "golden" $ do
      let check :: (HasCallStack) => (Patch PatchTag, Value) -> Expectation
          check (hs, js) = Right (toJSON hs) `shouldBe` lowerAllCaseInsensitiveThingsInPatch js

      check
        `mapM_` [ ( Patch [PatchOpAdd (Just $ AttrPath Nothing (AttrName "userName") Nothing) (String "testuser")],
                    [aesonQQ|
                    { "schemaS": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                      "operATIONS": [
                        { "oP": "add",
                          "pATh": "userName",
                          "vaLUE": "testuser"
                        }
                      ]
                    }
                    |]
                  ),
                  ( Patch [PatchOpReplace Nothing (String "this won't work in applyPatch")],
                    [aesonQQ|
                    { "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                      "operations": [
                        { "oP": "replace",
                          "vaLUE": "this won't work in applyPatch"
                        }
                      ]
                    }
                    |]
                  ),
                  ( Patch [PatchOpRemove (AttrPath (Just User20) (AttrName "userName") Nothing)],
                    [aesonQQ|
                    { "Schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
                      "Operations": [
                        { "op": "remove",
                          "path": "urn:ietf:params:scim:schemas:core:2.0:User:userName"
                        }
                      ]
                    }
                    |]
                  )
                ]

  describe "applyPatch" $ do
    prop "roundtrip (generate two users/groups, diff them, apply the patch, compare)" $
      \(barbie :: User (TestTag Text () () NoUserExtra)) changedWant ->
        let patchOp :: Patch (TestTag Text () () NoUserExtra)
            patchOp = todo -- PatchOp (AD.diff (toJSON barbie) (toJSON changedWant))
         in applyPatch patchOp barbie === Right changedWant

    it "throws expected error when patched object doesn't parse" $ do
      () <- todo
      True `shouldBe` False

    it "discards all paths that don't match the user/group schema" $ do
      _ <- todo
      True `shouldBe` False

    it "Throws error when trying to update immutable / readOnly values" $ do
      -- https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
      _ <- todo
      True `shouldBe` False

instance Arbitrary (User (TestTag Text () () NoUserExtra)) where
  -- TODO: move this to test module in library.
  arbitrary =
    {-  do
      userName <- undefined -- Gen.text (Range.constant 1 20) Gen.unicode
      externalId <- undefined -- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
      displayName <- undefined -- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
      active <- undefined -- Gen.maybe $ ScimBool <$> Gen.bool
      pure (empty [User20] userName NoUserExtra) {externalId = externalId}
     -}
    undefined
