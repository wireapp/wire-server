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

module Test.Wire.API.Golden.Generated.Feature_team where

import Data.Domain
import Imports
import Wire.API.Team.Feature

testObject_Feature_team_1 :: Feature AppLockConfig
testObject_Feature_team_1 = Feature FeatureStatusEnabled (AppLockConfig (EnforceAppLock False) (-98))

testObject_Feature_team_2 :: Feature AppLockConfig
testObject_Feature_team_2 = Feature FeatureStatusEnabled (AppLockConfig (EnforceAppLock True) 0)

testObject_Feature_team_3 :: Feature AppLockConfig
testObject_Feature_team_3 = Feature FeatureStatusEnabled (AppLockConfig (EnforceAppLock True) 111)

testObject_Feature_team_4 :: Feature SelfDeletingMessagesConfig
testObject_Feature_team_4 = Feature FeatureStatusEnabled (SelfDeletingMessagesConfig (-97))

testObject_Feature_team_5 :: Feature SelfDeletingMessagesConfig
testObject_Feature_team_5 = Feature FeatureStatusEnabled (SelfDeletingMessagesConfig 0)

testObject_Feature_team_6 :: Feature SelfDeletingMessagesConfig
testObject_Feature_team_6 = Feature FeatureStatusEnabled (SelfDeletingMessagesConfig 77)

testObject_Feature_team_7 :: Feature ClassifiedDomainsConfig
testObject_Feature_team_7 = Feature FeatureStatusEnabled (ClassifiedDomainsConfig [])

testObject_Feature_team_8 :: Feature ClassifiedDomainsConfig
testObject_Feature_team_8 = Feature FeatureStatusEnabled (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"])

testObject_Feature_team_9 :: Feature ClassifiedDomainsConfig
testObject_Feature_team_9 = Feature FeatureStatusEnabled (ClassifiedDomainsConfig [Domain "test.foobar"])

testObject_Feature_team_10 :: Feature SSOConfig
testObject_Feature_team_10 = Feature FeatureStatusDisabled SSOConfig

testObject_Feature_team_11 :: Feature SearchVisibilityAvailableConfig
testObject_Feature_team_11 = Feature FeatureStatusEnabled SearchVisibilityAvailableConfig

testObject_Feature_team_12 :: Feature ValidateSAMLEmailsConfig
testObject_Feature_team_12 = Feature FeatureStatusDisabled ValidateSAMLEmailsConfig

testObject_Feature_team_13 :: Feature DigitalSignaturesConfig
testObject_Feature_team_13 = Feature FeatureStatusEnabled DigitalSignaturesConfig

testObject_Feature_team_14 :: Feature ConferenceCallingConfig
testObject_Feature_team_14 = Feature FeatureStatusDisabled (ConferenceCallingConfig One2OneCallsSft)

testObject_Feature_team_15 :: Feature GuestLinksConfig
testObject_Feature_team_15 = Feature FeatureStatusEnabled GuestLinksConfig

testObject_Feature_team_16 :: Feature SndFactorPasswordChallengeConfig
testObject_Feature_team_16 = Feature FeatureStatusDisabled SndFactorPasswordChallengeConfig

testObject_Feature_team_17 :: Feature SearchVisibilityInboundConfig
testObject_Feature_team_17 = Feature FeatureStatusEnabled SearchVisibilityInboundConfig
