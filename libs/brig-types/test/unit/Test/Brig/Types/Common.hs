{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- | This is where currently all the json roundtrip tests happen for brig-types and
-- galley-types.
module Test.Brig.Types.Common where

import Brig.Types.Common
import Brig.Types.Team.LegalHold
import Brig.Types.Test.Arbitrary ()
import Data.Aeson
import Galley.Types
import Galley.Types.Teams
import Galley.Types.Teams.SSO
import Galley.Types.Teams.SearchVisibility
import Imports
import Test.Brig.Roundtrip (testRoundTrip)
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.Arbitrary ()

-- NB: validateEveryToJSON from servant-swagger doesn't render these tests unnecessary!

tests :: TestTree
tests =
  testGroup
    "Common (types vs. aeson)"
    [ testRoundTrip @Name,
      testRoundTrip @ColourId,
      testRoundTrip @Email,
      testRoundTrip @Phone,
      testRoundTrip @UserIdentity,
      testRoundTrip @UserSSOId,
      testRoundTrip @AssetSize,
      testRoundTrip @Asset,
      testRoundTrip @ExcludedPrefix,
      testRoundTrip @ManagedBy,
      testRoundTrip @TeamMemberDeleteData,
      testRoundTrip @LegalHoldStatus,
      testRoundTrip @LegalHoldTeamConfig,
      testRoundTrip @NewLegalHoldService,
      testRoundTrip @LegalHoldService,
      testRoundTrip @ViewLegalHoldService,
      testRoundTrip @NewLegalHoldClient,
      testRoundTrip @RequestNewLegalHoldClient,
      testRoundTrip @UserLegalHoldStatusResponse,
      testRoundTrip @LegalHoldServiceConfirm,
      testRoundTrip @LegalHoldClientRequest,
      testRoundTrip @RemoveLegalHoldSettingsRequest,
      testRoundTrip @DisableLegalHoldForUserRequest,
      testRoundTrip @ApproveLegalHoldForUserRequest,
      testRoundTrip @SSOStatus,
      testRoundTrip @SSOTeamConfig,
      testRoundTrip @TeamSearchVisibility,
      testRoundTrip @TeamSearchVisibilityView,
      testRoundTrip @TeamSearchVisibilityAvailable,
      testRoundTrip @TeamSearchVisibilityAvailableView,
      testRoundTrip @CustomBackend,
      testRoundTrip @FeatureFlags,
      testCase "{} is a valid TeamMemberDeleteData" $ do
        assertEqual "{}" (Right $ newTeamMemberDeleteData Nothing) (eitherDecode "{}")
    ]

instance Arbitrary FeatureFlags where
  arbitrary =
    FeatureFlags
      <$> Test.Tasty.QuickCheck.elements [minBound ..]
      <*> Test.Tasty.QuickCheck.elements [minBound ..]
      <*> Test.Tasty.QuickCheck.elements [minBound ..]
