{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Team.Feature (tests) where

import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Team.Feature

tests :: TestTree
tests =
  testGroup
    "Wire.API.Team.Feature"
    [ testCase "no lock status in DB" testComputeFeatureConfigForTeamUserLsIsNothing,
      testCase "feature is locked in DB" testComputeFeatureConfigForTeamUserLocked,
      testCase "feature is unlocked in DB but has no feature status" testComputeFeatureConfigForTeamUserUnlocked,
      testCase "feature is unlocked in DB and has feature status" testComputeFeatureConfigForTeamWithDbStatus
    ]

testComputeFeatureConfigForTeamUserLsIsNothing :: Assertion
testComputeFeatureConfigForTeamUserLsIsNothing = do
  let mStatusDb = undefined
  let mLockStatusDb = Nothing
  let defStatus =
        withStatus
          FeatureStatusEnabled
          LockStatusLocked
          ExposeInvitationURLsToTeamAdminConfig
          FeatureTTLUnlimited
  let expected = defStatus
  let actual = computeFeatureConfigForTeamUser @ExposeInvitationURLsToTeamAdminConfig mStatusDb mLockStatusDb defStatus
  actual @?= expected

testComputeFeatureConfigForTeamUserLocked :: Assertion
testComputeFeatureConfigForTeamUserLocked = do
  let mStatusDb = undefined
  let mLockStatusDb = Just LockStatusLocked
  let defStatus =
        withStatus
          FeatureStatusEnabled
          LockStatusLocked
          ExposeInvitationURLsToTeamAdminConfig
          FeatureTTLUnlimited
  let expected = defStatus
  let actual = computeFeatureConfigForTeamUser @ExposeInvitationURLsToTeamAdminConfig mStatusDb mLockStatusDb defStatus
  actual @?= expected

testComputeFeatureConfigForTeamUserUnlocked :: Assertion
testComputeFeatureConfigForTeamUserUnlocked = do
  let mStatusDb = Nothing
  let mLockStatusDb = Just LockStatusUnlocked
  let defStatus =
        withStatus
          FeatureStatusEnabled
          LockStatusLocked
          ExposeInvitationURLsToTeamAdminConfig
          FeatureTTLUnlimited
  let expected = defStatus & setLockStatus LockStatusUnlocked
  let actual = computeFeatureConfigForTeamUser @ExposeInvitationURLsToTeamAdminConfig mStatusDb mLockStatusDb defStatus
  actual @?= expected

testComputeFeatureConfigForTeamWithDbStatus :: Assertion
testComputeFeatureConfigForTeamWithDbStatus = do
  let mStatusDb =
        Just . forgetLock $
          withStatus
            FeatureStatusDisabled
            LockStatusUnlocked
            ExposeInvitationURLsToTeamAdminConfig
            FeatureTTLUnlimited
  let mLockStatusDb = Just LockStatusUnlocked
  let defStatus = undefined
  let (Just expected) = withUnlocked <$> mStatusDb
  let actual = computeFeatureConfigForTeamUser @ExposeInvitationURLsToTeamAdminConfig mStatusDb mLockStatusDb defStatus
  actual @?= expected
