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

module Test.Wire.API.Golden.Generated.WithStatus'_team where

import Data.Domain
import Imports
import Wire.API.Team.Feature
  ( AppLockConfig (AppLockConfig),
    ClassifiedDomainsConfig (ClassifiedDomainsConfig),
    ConferenceCallingConfig (..),
    DigitalSignaturesConfig (..),
    EnforceAppLock (EnforceAppLock),
    FeatureStatus (FeatureStatusDisabled, FeatureStatusEnabled),
    GuestLinksConfig (..),
    LockStatus (LockStatusLocked, LockStatusUnlocked),
    SSOConfig (..),
    SearchVisibilityAvailableConfig (..),
    SearchVisibilityInboundConfig (..),
    SelfDeletingMessagesConfig (SelfDeletingMessagesConfig),
    SndFactorPasswordChallengeConfig (..),
    ValidateSAMLEmailsConfig (..),
    WithStatus' (WithStatus'),
  )

testObject_WithStatus'_team_1 :: WithStatus' AppLockConfig
testObject_WithStatus'_team_1 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (AppLockConfig (EnforceAppLock False) (-98)))

testObject_WithStatus'_team_2 :: WithStatus' AppLockConfig
testObject_WithStatus'_team_2 = WithStatus' Nothing Nothing (Just (AppLockConfig (EnforceAppLock True) 0))

testObject_WithStatus'_team_3 :: WithStatus' AppLockConfig
testObject_WithStatus'_team_3 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just (AppLockConfig (EnforceAppLock True) 111))

testObject_WithStatus'_team_4 :: WithStatus' SelfDeletingMessagesConfig
testObject_WithStatus'_team_4 = WithStatus' (Just FeatureStatusEnabled) Nothing (Just (SelfDeletingMessagesConfig (-97)))

testObject_WithStatus'_team_5 :: WithStatus' SelfDeletingMessagesConfig
testObject_WithStatus'_team_5 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (SelfDeletingMessagesConfig 0))

testObject_WithStatus'_team_6 :: WithStatus' SelfDeletingMessagesConfig
testObject_WithStatus'_team_6 = WithStatus' (Just FeatureStatusEnabled) Nothing (Just (SelfDeletingMessagesConfig 77))

testObject_WithStatus'_team_7 :: WithStatus' ClassifiedDomainsConfig
testObject_WithStatus'_team_7 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just (ClassifiedDomainsConfig []))

testObject_WithStatus'_team_8 :: WithStatus' ClassifiedDomainsConfig
testObject_WithStatus'_team_8 = WithStatus' Nothing (Just LockStatusLocked) (Just (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"]))

testObject_WithStatus'_team_9 :: WithStatus' ClassifiedDomainsConfig
testObject_WithStatus'_team_9 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just (ClassifiedDomainsConfig [Domain "test.foobar"]))

testObject_WithStatus'_team_10 :: WithStatus' SSOConfig
testObject_WithStatus'_team_10 = WithStatus' (Just FeatureStatusDisabled) (Just LockStatusLocked) (Just SSOConfig)

testObject_WithStatus'_team_11 :: WithStatus' SearchVisibilityAvailableConfig
testObject_WithStatus'_team_11 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just SearchVisibilityAvailableConfig)

testObject_WithStatus'_team_12 :: WithStatus' ValidateSAMLEmailsConfig
testObject_WithStatus'_team_12 = WithStatus' (Just FeatureStatusDisabled) Nothing (Just ValidateSAMLEmailsConfig)

testObject_WithStatus'_team_13 :: WithStatus' DigitalSignaturesConfig
testObject_WithStatus'_team_13 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusLocked) (Just DigitalSignaturesConfig)

testObject_WithStatus'_team_14 :: WithStatus' ConferenceCallingConfig
testObject_WithStatus'_team_14 = WithStatus' Nothing (Just LockStatusUnlocked) (Just ConferenceCallingConfig)

testObject_WithStatus'_team_15 :: WithStatus' GuestLinksConfig
testObject_WithStatus'_team_15 = WithStatus' (Just FeatureStatusEnabled) (Just LockStatusUnlocked) (Just GuestLinksConfig)

testObject_WithStatus'_team_16 :: WithStatus' SndFactorPasswordChallengeConfig
testObject_WithStatus'_team_16 = WithStatus' (Just FeatureStatusDisabled) (Just LockStatusUnlocked) (Just SndFactorPasswordChallengeConfig)

testObject_WithStatus'_team_17 :: WithStatus' SearchVisibilityInboundConfig
testObject_WithStatus'_team_17 = WithStatus' (Just FeatureStatusEnabled) Nothing (Just SearchVisibilityInboundConfig)

testObject_WithStatus'_team_18 :: WithStatus' GuestLinksConfig
testObject_WithStatus'_team_18 = WithStatus' (Just FeatureStatusEnabled) Nothing Nothing

testObject_WithStatus'_team_19 :: WithStatus' SelfDeletingMessagesConfig
testObject_WithStatus'_team_19 = WithStatus' Nothing (Just LockStatusUnlocked) Nothing
