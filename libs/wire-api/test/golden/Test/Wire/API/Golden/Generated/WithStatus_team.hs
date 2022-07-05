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

module Test.Wire.API.Golden.Generated.WithStatus_team where

import Data.Domain
import Imports
import Wire.API.Team.Feature

testObject_WithStatus_team_1 :: WithStatus AppLockConfig
testObject_WithStatus_team_1 = WithStatus FeatureStatusEnabled LockStatusUnlocked (AppLockConfig (EnforceAppLock False) (-98))

testObject_WithStatus_team_2 :: WithStatus AppLockConfig
testObject_WithStatus_team_2 = WithStatus FeatureStatusEnabled LockStatusUnlocked (AppLockConfig (EnforceAppLock True) 0)

testObject_WithStatus_team_3 :: WithStatus AppLockConfig
testObject_WithStatus_team_3 = WithStatus FeatureStatusEnabled LockStatusLocked (AppLockConfig (EnforceAppLock True) 111)

testObject_WithStatus_team_4 :: WithStatus SelfDeletingMessagesConfig
testObject_WithStatus_team_4 = WithStatus FeatureStatusEnabled LockStatusUnlocked (SelfDeletingMessagesConfig (-97))

testObject_WithStatus_team_5 :: WithStatus SelfDeletingMessagesConfig
testObject_WithStatus_team_5 = WithStatus FeatureStatusEnabled LockStatusUnlocked (SelfDeletingMessagesConfig 0)

testObject_WithStatus_team_6 :: WithStatus SelfDeletingMessagesConfig
testObject_WithStatus_team_6 = WithStatus FeatureStatusEnabled LockStatusLocked (SelfDeletingMessagesConfig 77)

testObject_WithStatus_team_7 :: WithStatus ClassifiedDomainsConfig
testObject_WithStatus_team_7 = WithStatus FeatureStatusEnabled LockStatusLocked (ClassifiedDomainsConfig [])

testObject_WithStatus_team_8 :: WithStatus ClassifiedDomainsConfig
testObject_WithStatus_team_8 = WithStatus FeatureStatusEnabled LockStatusLocked (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"])

testObject_WithStatus_team_9 :: WithStatus ClassifiedDomainsConfig
testObject_WithStatus_team_9 = WithStatus FeatureStatusEnabled LockStatusUnlocked (ClassifiedDomainsConfig [Domain "test.foobar"])

testObject_WithStatus_team_10 :: WithStatus SSOConfig
testObject_WithStatus_team_10 = WithStatus FeatureStatusDisabled LockStatusLocked SSOConfig

testObject_WithStatus_team_11 :: WithStatus SearchVisibilityAvailableConfig
testObject_WithStatus_team_11 = WithStatus FeatureStatusEnabled LockStatusLocked SearchVisibilityAvailableConfig

testObject_WithStatus_team_12 :: WithStatus ValidateSAMLEmailsConfig
testObject_WithStatus_team_12 = WithStatus FeatureStatusDisabled LockStatusLocked ValidateSAMLEmailsConfig

testObject_WithStatus_team_13 :: WithStatus DigitalSignaturesConfig
testObject_WithStatus_team_13 = WithStatus FeatureStatusEnabled LockStatusLocked DigitalSignaturesConfig

testObject_WithStatus_team_14 :: WithStatus ConferenceCallingConfig
testObject_WithStatus_team_14 = WithStatus FeatureStatusDisabled LockStatusUnlocked ConferenceCallingConfig

testObject_WithStatus_team_15 :: WithStatus GuestLinksConfig
testObject_WithStatus_team_15 = WithStatus FeatureStatusEnabled LockStatusUnlocked GuestLinksConfig

testObject_WithStatus_team_16 :: WithStatus SndFactorPasswordChallengeConfig
testObject_WithStatus_team_16 = WithStatus FeatureStatusDisabled LockStatusUnlocked SndFactorPasswordChallengeConfig

testObject_WithStatus_team_17 :: WithStatus SearchVisibilityInboundConfig
testObject_WithStatus_team_17 = WithStatus FeatureStatusEnabled LockStatusUnlocked SearchVisibilityInboundConfig
