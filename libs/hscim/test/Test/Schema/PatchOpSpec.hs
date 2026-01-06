{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as T
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Email.Parser
import Web.Scim.AttrName
import Web.Scim.Filter
import Web.Scim.Schema.Common
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.Schema
import Web.Scim.Schema.User
import Web.Scim.Schema.User.Email
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
        `mapM_` [ ( Patch
                      [ PatchOpAdd
                          (Just (ValuePath (topLevelAttrPath "userName") Nothing))
                          (String "testuser")
                      ],
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
                  ( Patch
                      [ PatchOpRemove
                          (ValuePath (AttrPath (Just User20) (AttrName "userName") Nothing) Nothing)
                      ],
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
      \(barbie :: User PatchTag) (changedWant :: User PatchTag) ->
        let patchOp :: Patch PatchTag
            patchOp =
              jsonPatchToScimPatch (AD.diff (toJSON barbie) (toJSON changedWant)) (toJSON barbie)
                & either (error . show) Imports.id

            go =
              let j = scimPatchToJsonPatch patchOp (toJSON barbie)
               in jsonPatchToScimPatch j (toJSON barbie)
         in go === Right patchOp

    prop "roundtrip (generate two users/groups, diff them, apply the patch, compare)" $
      \(barbie :: User PatchTag) changedWant ->
        let patchOp :: Patch PatchTag
            patchOp =
              jsonPatchToScimPatch (AD.diff (toJSON barbie) (toJSON changedWant)) (toJSON barbie)
                & either (error . show) Imports.id
         in applyPatch patchOp barbie === Right changedWant

    focus . prop "arrFilterToIndices/arrIndexToFilter roundtrip on singleton match" $
      forAll genArrFilterCase $ \(arr, fltr, ix) ->
        let indices = arrFilterToIndices fltr arr
            fltr' = arrIndexToFilter ix arr
         in indices === [ix]
              .&&. arrFilterToIndices fltr' arr === indices

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

----------------------------------------------------------------------
-- Arbitrary -- TODO: move to Web.Scim.Test.Something

instance Arbitrary (User PatchTag) where
  arbitrary = do
    userName <- T.pack <$> listOf1 arbitrary
    externalId <- oneof [pure Nothing, Just . T.pack <$> listOf1 arbitrary]
    displayName <- oneof [pure Nothing, Just . T.pack <$> listOf1 arbitrary]
    active <- ScimBool <$$> arbitrary
    emails <- listOf arbitrary
    roles <- T.pack <$$> listOf1 arbitrary
    pure (empty @PatchTag [User20] userName mempty) {externalId, displayName, active, emails, roles}

instance Arbitrary Email where
  arbitrary = do
    typ <- elements (Nothing : (Just <$> ["work", "mobile", "yellow"]))
    value <- EmailAddress . (`unsafeEmailAddress` "example.com") . BS.pack <$> listOf1 arbitrary
    primary <- ScimBool <$$> arbitrary
    pure Email {..}

genArrFilterCase :: Gen ([Value], Filter, Int)
genArrFilterCase = do
  compVal <- genCompValue
  let fltr = FilterAttrCompare (AttrPath Nothing "value" Nothing) OpEq compVal
  useObject <- arbitrary
  keyVariant <- elements ["value", "VALUE", "Value"]
  let matchingValue =
        if useObject
          then Object (KeyMap.singleton keyVariant (compValueToValue compVal))
          else compValueToValue compVal
  prefix <- listOf (genNonMatchingValue compVal)
  suffix <- listOf (genNonMatchingValue compVal)
  let ix = length prefix
  pure (prefix <> [matchingValue] <> suffix, fltr, ix)

genCompValue :: Gen CompValue
genCompValue =
  oneof
    [ ValString <$> genText,
      ValNumber <$> genScientific,
      ValBool <$> arbitrary,
      pure ValNull
    ]

genNonMatchingValue :: CompValue -> Gen Value
genNonMatchingValue compVal = oneof [genPrimitive, genObject]
  where
    genPrimitive = compValueToValue <$> genDifferentCompValue compVal
    genObject = do
      keyVariant <- elements ["value", "VALUE", "Value"]
      val <- compValueToValue <$> genDifferentCompValue compVal
      pure (Object (KeyMap.singleton keyVariant val))

genDifferentCompValue :: CompValue -> Gen CompValue
genDifferentCompValue compVal = case compVal of
  ValString s ->
    ValString <$> suchThat genText (\t -> CI.foldCase t /= CI.foldCase s)
  ValNumber n -> ValNumber <$> suchThat genScientific (/= n)
  ValBool b -> pure (ValBool (not b))
  ValNull -> oneof [ValBool <$> arbitrary, ValNumber <$> genScientific, ValString <$> genText]

compValueToValue :: CompValue -> Value
compValueToValue = \case
  ValNull -> Null
  ValBool b -> Bool b
  ValNumber n -> Number n
  ValString s -> String s

genText :: Gen Text
genText = T.pack <$> listOf1 (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))

genScientific :: Gen Scientific
genScientific = do
  coeff <- arbitrary :: Gen Integer
  exp10 <- chooseInt (-6, 6)
  pure (scientific coeff exp10)
