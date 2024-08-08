{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.WithStatusPatch_team where

import Data.Domain
import Imports
import Wire.API.Team.Feature hiding (withStatus)

testObject_WithStatusPatch_team_1 :: WithStatusPatch AppLockConfig
testObject_WithStatusPatch_team_1 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (AppLockConfig (EnforceAppLock False) (-98)))

testObject_WithStatusPatch_team_2 :: WithStatusPatch AppLockConfig
testObject_WithStatusPatch_team_2 = withStatus Nothing Nothing (Just (AppLockConfig (EnforceAppLock True) 0))

testObject_WithStatusPatch_team_3 :: WithStatusPatch AppLockConfig
testObject_WithStatusPatch_team_3 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just (AppLockConfig (EnforceAppLock True) 111))

testObject_WithStatusPatch_team_4 :: WithStatusPatch SelfDeletingMessagesConfig
testObject_WithStatusPatch_team_4 = withStatus (Just FeatureStatusEnabled) Nothing (Just (SelfDeletingMessagesConfig (-97)))

testObject_WithStatusPatch_team_5 :: WithStatusPatch SelfDeletingMessagesConfig
testObject_WithStatusPatch_team_5 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (SelfDeletingMessagesConfig 0))

testObject_WithStatusPatch_team_6 :: WithStatusPatch SelfDeletingMessagesConfig
testObject_WithStatusPatch_team_6 = withStatus (Just FeatureStatusEnabled) Nothing (Just (SelfDeletingMessagesConfig 77))

testObject_WithStatusPatch_team_7 :: WithStatusPatch ClassifiedDomainsConfig
testObject_WithStatusPatch_team_7 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just (ClassifiedDomainsConfig []))

testObject_WithStatusPatch_team_8 :: WithStatusPatch ClassifiedDomainsConfig
testObject_WithStatusPatch_team_8 = withStatus Nothing (Just LockStatusLocked) (Just (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"]))

testObject_WithStatusPatch_team_9 :: WithStatusPatch ClassifiedDomainsConfig
testObject_WithStatusPatch_team_9 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (ClassifiedDomainsConfig [Domain "test.foobar"]))

testObject_WithStatusPatch_team_10 :: WithStatusPatch SSOConfig
testObject_WithStatusPatch_team_10 = withStatus (Just FeatureStatusDisabled) (Just LockStatusLocked) (Just SSOConfig)

testObject_WithStatusPatch_team_11 :: WithStatusPatch SearchVisibilityAvailableConfig
testObject_WithStatusPatch_team_11 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just SearchVisibilityAvailableConfig)

testObject_WithStatusPatch_team_12 :: WithStatusPatch ValidateSAMLEmailsConfig
testObject_WithStatusPatch_team_12 = withStatus (Just FeatureStatusDisabled) Nothing (Just ValidateSAMLEmailsConfig)

testObject_WithStatusPatch_team_13 :: WithStatusPatch DigitalSignaturesConfig
testObject_WithStatusPatch_team_13 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just DigitalSignaturesConfig)

testObject_WithStatusPatch_team_14 :: WithStatusPatch ConferenceCallingConfig
testObject_WithStatusPatch_team_14 = withStatus Nothing (Just LockStatusUnlocked) (Just (ConferenceCallingConfig One2OneCallsSft))

testObject_WithStatusPatch_team_15 :: WithStatusPatch GuestLinksConfig
testObject_WithStatusPatch_team_15 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just GuestLinksConfig)

testObject_WithStatusPatch_team_16 :: WithStatusPatch SndFactorPasswordChallengeConfig
testObject_WithStatusPatch_team_16 = withStatus (Just FeatureStatusDisabled) (Just LockStatusUnlocked) (Just SndFactorPasswordChallengeConfig)

testObject_WithStatusPatch_team_17 :: WithStatusPatch SearchVisibilityInboundConfig
testObject_WithStatusPatch_team_17 = withStatus (Just FeatureStatusEnabled) Nothing (Just SearchVisibilityInboundConfig)

testObject_WithStatusPatch_team_18 :: WithStatusPatch GuestLinksConfig
testObject_WithStatusPatch_team_18 = withStatus (Just FeatureStatusEnabled) Nothing Nothing

testObject_WithStatusPatch_team_19 :: WithStatusPatch SelfDeletingMessagesConfig
testObject_WithStatusPatch_team_19 = withStatus Nothing (Just LockStatusUnlocked) Nothing

withStatus :: Maybe FeatureStatus -> Maybe LockStatus -> Maybe cfg -> WithStatusPatch cfg
withStatus fs ls cfg = withStatus' fs ls cfg (Just FeatureTTLUnlimited)
