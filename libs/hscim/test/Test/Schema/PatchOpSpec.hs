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

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec
import Web.Scim.Test.Util (TestTag)

type PatchTag = TestTag Text () () UserExtraPatch

type UserExtraPatch = KeyMap.KeyMap Text

spec :: Spec
spec = do
  describe "PatchOp" $ do
    it "golden" $ do
      pending

    it "roundtrip" $ do
      -- we don't need a property here, one sample is enough: AD.Patch
      -- is tested in aeson-diff.
      pending

    it "Operation attributes and value attributes are case-insensitive" $ do
      pending

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
        pending

      for_ examples2 $ \ex -> it ex $ do
        pendingWith "FUTUREWORK"

    it "rejects unsupported operations with the proper error (not could-not-parse)" $ do
      pending

  describe "applyPatch" $ do
    it "prop: roundtrip (generate two users/groups, diff them, apply the patch, compare)" $ do
      -- also patch stuff in extra
      pending

    it "throws error when patched object doesn't parse" $ do
      pending

    it "discards all paths that don't match the user/group schema" $ do
      pending

    it "throws error when trying to update immutable / readOnly values" $ do
      -- https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
      pendingWith "FUTUREWORK"
