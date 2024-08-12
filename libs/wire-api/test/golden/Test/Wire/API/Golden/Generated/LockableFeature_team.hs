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

module Test.Wire.API.Golden.Generated.LockableFeature_team where

import Data.ByteString.Conversion (parser, runParser)
import Data.Domain
import Data.Misc
import Imports
import Wire.API.Team.Feature

testObject_LockableFeature_team_1 :: LockableFeature AppLockConfig
testObject_LockableFeature_team_1 = LockableFeature FeatureStatusEnabled LockStatusUnlocked (AppLockConfig (EnforceAppLock False) (-98))

testObject_LockableFeature_team_2 :: LockableFeature AppLockConfig
testObject_LockableFeature_team_2 = LockableFeature FeatureStatusEnabled LockStatusUnlocked (AppLockConfig (EnforceAppLock True) 0)

testObject_LockableFeature_team_3 :: LockableFeature AppLockConfig
testObject_LockableFeature_team_3 = LockableFeature FeatureStatusEnabled LockStatusLocked (AppLockConfig (EnforceAppLock True) 111)

testObject_LockableFeature_team_4 :: LockableFeature SelfDeletingMessagesConfig
testObject_LockableFeature_team_4 = LockableFeature FeatureStatusEnabled LockStatusUnlocked (SelfDeletingMessagesConfig (-97))

testObject_LockableFeature_team_5 :: LockableFeature SelfDeletingMessagesConfig
testObject_LockableFeature_team_5 = LockableFeature FeatureStatusEnabled LockStatusUnlocked (SelfDeletingMessagesConfig 0)

testObject_LockableFeature_team_6 :: LockableFeature SelfDeletingMessagesConfig
testObject_LockableFeature_team_6 = LockableFeature FeatureStatusEnabled LockStatusLocked (SelfDeletingMessagesConfig 77)

testObject_LockableFeature_team_7 :: LockableFeature ClassifiedDomainsConfig
testObject_LockableFeature_team_7 = LockableFeature FeatureStatusEnabled LockStatusLocked (ClassifiedDomainsConfig [])

testObject_LockableFeature_team_8 :: LockableFeature ClassifiedDomainsConfig
testObject_LockableFeature_team_8 = LockableFeature FeatureStatusEnabled LockStatusLocked (ClassifiedDomainsConfig [Domain "example.com", Domain "test.foobar"])

testObject_LockableFeature_team_9 :: LockableFeature ClassifiedDomainsConfig
testObject_LockableFeature_team_9 = LockableFeature FeatureStatusEnabled LockStatusUnlocked (ClassifiedDomainsConfig [Domain "test.foobar"])

testObject_LockableFeature_team_10 :: LockableFeature SSOConfig
testObject_LockableFeature_team_10 = LockableFeature FeatureStatusDisabled LockStatusLocked SSOConfig

testObject_LockableFeature_team_11 :: LockableFeature SearchVisibilityAvailableConfig
testObject_LockableFeature_team_11 = LockableFeature FeatureStatusEnabled LockStatusLocked SearchVisibilityAvailableConfig

testObject_LockableFeature_team_12 :: LockableFeature ValidateSAMLEmailsConfig
testObject_LockableFeature_team_12 = LockableFeature FeatureStatusDisabled LockStatusLocked ValidateSAMLEmailsConfig

testObject_LockableFeature_team_13 :: LockableFeature DigitalSignaturesConfig
testObject_LockableFeature_team_13 = LockableFeature FeatureStatusEnabled LockStatusLocked DigitalSignaturesConfig

testObject_LockableFeature_team_14 :: LockableFeature ConferenceCallingConfig
testObject_LockableFeature_team_14 = LockableFeature FeatureStatusDisabled LockStatusUnlocked (ConferenceCallingConfig One2OneCallsTurn)

testObject_LockableFeature_team_15 :: LockableFeature GuestLinksConfig
testObject_LockableFeature_team_15 = LockableFeature FeatureStatusEnabled LockStatusUnlocked GuestLinksConfig

testObject_LockableFeature_team_16 :: LockableFeature SndFactorPasswordChallengeConfig
testObject_LockableFeature_team_16 = LockableFeature FeatureStatusDisabled LockStatusUnlocked SndFactorPasswordChallengeConfig

testObject_LockableFeature_team_17 :: LockableFeature SearchVisibilityInboundConfig
testObject_LockableFeature_team_17 = LockableFeature FeatureStatusEnabled LockStatusUnlocked SearchVisibilityInboundConfig

testObject_LockableFeature_team_18 :: LockableFeature MlsE2EIdConfig
testObject_LockableFeature_team_18 =
  LockableFeature
    FeatureStatusEnabled
    LockStatusLocked
    ( MlsE2EIdConfig
        (fromIntegral @Int (60 * 60 * 24))
        Nothing
        (either (\e -> error (show e)) Just $ parseHttpsUrl "https://example.com")
        False
    )

parseHttpsUrl :: ByteString -> Either String HttpsUrl
parseHttpsUrl url = runParser parser url

testObject_LockableFeature_team_19 :: LockableFeature MlsE2EIdConfig
testObject_LockableFeature_team_19 =
  LockableFeature
    FeatureStatusEnabled
    LockStatusLocked
    ( MlsE2EIdConfig
        (fromIntegral @Int (60 * 60 * 24))
        (either (\e -> error (show e)) Just $ parseHttpsUrl "https://example.com")
        Nothing
        True
    )
