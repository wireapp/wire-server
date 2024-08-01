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

module Test.Wire.API.Golden.Generated.LockableFeaturePatch_team where

import Data.Domain
import Imports
import Wire.API.Team.Feature hiding (withStatus)

testObject_LockableFeaturePatch_team_1 :: LockableFeaturePatch AppLockConfig
testObject_LockableFeaturePatch_team_1 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (AppLockConfig (EnforceAppLock False) (-98)))

testObject_LockableFeaturePatch_team_2 :: LockableFeaturePatch AppLockConfig
testObject_LockableFeaturePatch_team_2 = withStatus Nothing Nothing (Just (AppLockConfig (EnforceAppLock True) 0))

testObject_LockableFeaturePatch_team_3 :: LockableFeaturePatch AppLockConfig
testObject_LockableFeaturePatch_team_3 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just (AppLockConfig (EnforceAppLock True) 111))

testObject_LockableFeaturePatch_team_4 :: LockableFeaturePatch SelfDeletingMessagesConfig
testObject_LockableFeaturePatch_team_4 = withStatus (Just FeatureStatusEnabled) Nothing (Just (SelfDeletingMessagesConfig (-97)))

testObject_LockableFeaturePatch_team_5 :: LockableFeaturePatch SelfDeletingMessagesConfig
testObject_LockableFeaturePatch_team_5 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (SelfDeletingMessagesConfig 0))

testObject_LockableFeaturePatch_team_6 :: LockableFeaturePatch SelfDeletingMessagesConfig
testObject_LockableFeaturePatch_team_6 = withStatus (Just FeatureStatusEnabled) Nothing (Just (SelfDeletingMessagesConfig 77))

testObject_LockableFeaturePatch_team_7 :: LockableFeaturePatch ClassifiedDomainsConfig
testObject_LockableFeaturePatch_team_7 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just (ClassifiedDomainsConfig []))

testObject_LockableFeaturePatch_team_8 :: LockableFeaturePatch ClassifiedDomainsConfig
testObject_LockableFeaturePatch_team_8 = withStatus Nothing (Just LockStatusLocked) (Just (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"]))

testObject_LockableFeaturePatch_team_9 :: LockableFeaturePatch ClassifiedDomainsConfig
testObject_LockableFeaturePatch_team_9 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (ClassifiedDomainsConfig [Domain "test.foobar"]))

testObject_LockableFeaturePatch_team_10 :: LockableFeaturePatch SSOConfig
testObject_LockableFeaturePatch_team_10 = withStatus (Just FeatureStatusDisabled) (Just LockStatusLocked) (Just SSOConfig)

testObject_LockableFeaturePatch_team_11 :: LockableFeaturePatch SearchVisibilityAvailableConfig
testObject_LockableFeaturePatch_team_11 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just SearchVisibilityAvailableConfig)

testObject_LockableFeaturePatch_team_12 :: LockableFeaturePatch ValidateSAMLEmailsConfig
testObject_LockableFeaturePatch_team_12 = withStatus (Just FeatureStatusDisabled) Nothing (Just ValidateSAMLEmailsConfig)

testObject_LockableFeaturePatch_team_13 :: LockableFeaturePatch DigitalSignaturesConfig
testObject_LockableFeaturePatch_team_13 = withStatus (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just DigitalSignaturesConfig)

testObject_LockableFeaturePatch_team_14 :: LockableFeaturePatch ConferenceCallingConfig
testObject_LockableFeaturePatch_team_14 = withStatus Nothing (Just LockStatusUnlocked) (Just (ConferenceCallingConfig One2OneCallsSft))

testObject_LockableFeaturePatch_team_15 :: LockableFeaturePatch GuestLinksConfig
testObject_LockableFeaturePatch_team_15 = withStatus (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just GuestLinksConfig)

testObject_LockableFeaturePatch_team_16 :: LockableFeaturePatch SndFactorPasswordChallengeConfig
testObject_LockableFeaturePatch_team_16 = withStatus (Just FeatureStatusDisabled) (Just LockStatusUnlocked) (Just SndFactorPasswordChallengeConfig)

testObject_LockableFeaturePatch_team_17 :: LockableFeaturePatch SearchVisibilityInboundConfig
testObject_LockableFeaturePatch_team_17 = withStatus (Just FeatureStatusEnabled) Nothing (Just SearchVisibilityInboundConfig)

testObject_LockableFeaturePatch_team_18 :: LockableFeaturePatch GuestLinksConfig
testObject_LockableFeaturePatch_team_18 = withStatus (Just FeatureStatusEnabled) Nothing Nothing

testObject_LockableFeaturePatch_team_19 :: LockableFeaturePatch SelfDeletingMessagesConfig
testObject_LockableFeaturePatch_team_19 = withStatus Nothing (Just LockStatusUnlocked) Nothing

withStatus :: Maybe FeatureStatus -> Maybe LockStatus -> Maybe cfg -> LockableFeaturePatch cfg
withStatus fs ls cfg = withStatus' fs ls cfg (Just FeatureTTLUnlimited)
