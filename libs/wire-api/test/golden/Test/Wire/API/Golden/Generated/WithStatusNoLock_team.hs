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

module Test.Wire.API.Golden.Generated.WithStatusNoLock_team where

import Data.Domain
import Imports
import Wire.API.Team.Feature

testObject_WithStatusNoLock_team_1 :: WithStatusNoLock AppLockConfig
testObject_WithStatusNoLock_team_1 = WithStatusNoLock FeatureStatusEnabled (AppLockConfig (EnforceAppLock False) (-98)) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_2 :: WithStatusNoLock AppLockConfig
testObject_WithStatusNoLock_team_2 = WithStatusNoLock FeatureStatusEnabled (AppLockConfig (EnforceAppLock True) 0) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_3 :: WithStatusNoLock AppLockConfig
testObject_WithStatusNoLock_team_3 = WithStatusNoLock FeatureStatusEnabled (AppLockConfig (EnforceAppLock True) 111) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_4 :: WithStatusNoLock SelfDeletingMessagesConfig
testObject_WithStatusNoLock_team_4 = WithStatusNoLock FeatureStatusEnabled (SelfDeletingMessagesConfig (-97)) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_5 :: WithStatusNoLock SelfDeletingMessagesConfig
testObject_WithStatusNoLock_team_5 = WithStatusNoLock FeatureStatusEnabled (SelfDeletingMessagesConfig 0) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_6 :: WithStatusNoLock SelfDeletingMessagesConfig
testObject_WithStatusNoLock_team_6 = WithStatusNoLock FeatureStatusEnabled (SelfDeletingMessagesConfig 77) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_7 :: WithStatusNoLock ClassifiedDomainsConfig
testObject_WithStatusNoLock_team_7 = WithStatusNoLock FeatureStatusEnabled (ClassifiedDomainsConfig []) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_8 :: WithStatusNoLock ClassifiedDomainsConfig
testObject_WithStatusNoLock_team_8 = WithStatusNoLock FeatureStatusEnabled (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"]) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_9 :: WithStatusNoLock ClassifiedDomainsConfig
testObject_WithStatusNoLock_team_9 = WithStatusNoLock FeatureStatusEnabled (ClassifiedDomainsConfig [Domain "test.foobar"]) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_10 :: WithStatusNoLock SSOConfig
testObject_WithStatusNoLock_team_10 = WithStatusNoLock FeatureStatusDisabled SSOConfig FeatureTTLUnlimited

testObject_WithStatusNoLock_team_11 :: WithStatusNoLock SearchVisibilityAvailableConfig
testObject_WithStatusNoLock_team_11 = WithStatusNoLock FeatureStatusEnabled SearchVisibilityAvailableConfig FeatureTTLUnlimited

testObject_WithStatusNoLock_team_12 :: WithStatusNoLock ValidateSAMLEmailsConfig
testObject_WithStatusNoLock_team_12 = WithStatusNoLock FeatureStatusDisabled ValidateSAMLEmailsConfig FeatureTTLUnlimited

testObject_WithStatusNoLock_team_13 :: WithStatusNoLock DigitalSignaturesConfig
testObject_WithStatusNoLock_team_13 = WithStatusNoLock FeatureStatusEnabled DigitalSignaturesConfig FeatureTTLUnlimited

testObject_WithStatusNoLock_team_14 :: WithStatusNoLock ConferenceCallingConfig
testObject_WithStatusNoLock_team_14 = WithStatusNoLock FeatureStatusDisabled (ConferenceCallingConfig One2OneCallsSft) FeatureTTLUnlimited

testObject_WithStatusNoLock_team_15 :: WithStatusNoLock GuestLinksConfig
testObject_WithStatusNoLock_team_15 = WithStatusNoLock FeatureStatusEnabled GuestLinksConfig FeatureTTLUnlimited

testObject_WithStatusNoLock_team_16 :: WithStatusNoLock SndFactorPasswordChallengeConfig
testObject_WithStatusNoLock_team_16 = WithStatusNoLock FeatureStatusDisabled SndFactorPasswordChallengeConfig FeatureTTLUnlimited

testObject_WithStatusNoLock_team_17 :: WithStatusNoLock SearchVisibilityInboundConfig
testObject_WithStatusNoLock_team_17 = WithStatusNoLock FeatureStatusEnabled SearchVisibilityInboundConfig FeatureTTLUnlimited
