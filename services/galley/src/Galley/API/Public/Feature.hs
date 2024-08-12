{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

module Galley.API.Public.Feature where

import Galley.API.Teams
import Galley.API.Teams.Features
import Galley.API.Teams.Features.Get
import Galley.App
import Imports
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Feature
import Wire.API.Team.Feature

featureAPIGetPut :: forall cfg r. (_) => API (FeatureAPIGetPut cfg) r
featureAPIGetPut =
  mkNamedAPI @'("get", cfg) (getFeature . DoAuth)
    <@> mkNamedAPI @'("put", cfg) (setFeature . DoAuth)

featureAPI :: API FeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI @'("get", SSOConfig) (getFeature . DoAuth)
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> mkNamedAPI @"get-search-visibility" getSearchVisibility
    <@> mkNamedAPI @"set-search-visibility" (setSearchVisibility (featureEnabledForTeam @SearchVisibilityAvailableConfig))
    <@> mkNamedAPI @'("get", ValidateSAMLEmailsConfig) (getFeature . DoAuth)
    <@> mkNamedAPI @'("get", DigitalSignaturesConfig) (getFeature . DoAuth)
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> mkNamedAPI @'("get", ClassifiedDomainsConfig) (getFeature . DoAuth)
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> hoistAPI id featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> mkNamedAPI @'("get", MlsE2EIdConfig) (getFeature . DoAuth)
    <@> mkNamedAPI @"put-MlsE2EIdConfig@v5" (setFeature . DoAuth)
    <@> mkNamedAPI @'("put", MlsE2EIdConfig) (guardMlsE2EIdConfig (setFeature . DoAuth))
    <@> hoistAPI id featureAPIGetPut
    <@> hoistAPI id featureAPIGetPut
    <@> mkNamedAPI @'("get", LimitedEventFanoutConfig) (getFeature . DoAuth)
    <@> mkNamedAPI @"get-all-feature-configs-for-user" getAllFeatureConfigsForUser
    <@> mkNamedAPI @"get-all-feature-configs-for-team" getAllFeatureConfigsForTeam
    <@> deprecatedFeatureConfigAPI
    <@> deprecatedFeatureAPI

deprecatedFeatureConfigAPI :: API DeprecatedFeatureAPI GalleyEffects
deprecatedFeatureConfigAPI =
  mkNamedAPI @'("get-deprecated", SearchVisibilityAvailableConfig) (getFeature . DoAuth)
    <@> mkNamedAPI @'("put-deprecated", SearchVisibilityAvailableConfig) (setFeature . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", ValidateSAMLEmailsConfig) (getFeature . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", DigitalSignaturesConfig) (getFeature . DoAuth)

deprecatedFeatureAPI :: API (AllDeprecatedFeatureConfigAPI DeprecatedFeatureConfigs) GalleyEffects
deprecatedFeatureAPI =
  mkNamedAPI @'("get-config", LegalholdConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", SSOConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", SearchVisibilityAvailableConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", ValidateSAMLEmailsConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", DigitalSignaturesConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", AppLockConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", FileSharingConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", ClassifiedDomainsConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", ConferenceCallingConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", SelfDeletingMessagesConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", GuestLinksConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", SndFactorPasswordChallengeConfig) getSingleFeatureConfigForUser
    <@> mkNamedAPI @'("get-config", MLSConfig) getSingleFeatureConfigForUser
