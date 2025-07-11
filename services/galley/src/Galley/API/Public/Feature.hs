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
import Wire.API.Routes.Version
import Wire.API.Team.Feature

featureAPIGetPut :: forall cfg r. (_) => API (FeatureAPIGetPut cfg) r
featureAPIGetPut =
  mkNamedAPI @'("get", cfg) getFeature
    <@> mkNamedAPI @'("put", cfg) setFeature

featureAPI :: API FeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI @'("get", SSOConfig) getFeature
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> mkNamedAPI @"get-search-visibility" getSearchVisibility
    <@> mkNamedAPI @"set-search-visibility" (setSearchVisibility (featureEnabledForTeam @SearchVisibilityAvailableConfig))
    <@> mkNamedAPI @'("get", ValidateSAMLEmailsConfig) getFeature
    <@> mkNamedAPI @'("get", DigitalSignaturesConfig) getFeature
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> mkNamedAPI @'("get", ClassifiedDomainsConfig) getFeature
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> hoistAPI id featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> featureAPIGetPut
    <@> mkNamedAPI @'("get", MlsE2EIdConfig) getFeature
    <@> mkNamedAPI @"put-MlsE2EIdConfig@v5" setFeature
    <@> mkNamedAPI @'("put", MlsE2EIdConfig) (guardMlsE2EIdConfig setFeature)
    <@> hoistAPI id featureAPIGetPut
    <@> hoistAPI id featureAPIGetPut
    <@> mkNamedAPI @'("get", LimitedEventFanoutConfig) getFeature
    <@> mkNamedAPI @"get-all-feature-configs-for-user" getAllTeamFeaturesForUser
    <@> mkNamedAPI @"get-all-feature-configs-for-team" getAllTeamFeaturesForTeam
    <@> deprecatedFeatureConfigAPI
    <@> deprecatedFeatureAPI
    <@> mkNamedAPI @'("get", DomainRegistrationConfig) getFeature
    <@> featureAPIGetPut
    <@> featureAPIGetPut

deprecatedFeatureConfigAPI :: API DeprecatedFeatureAPI GalleyEffects
deprecatedFeatureConfigAPI =
  mkNamedAPI @'("get-deprecated", '(SearchVisibilityAvailableConfig, V2)) getFeature
    <@> mkNamedAPI @'("put-deprecated", '(SearchVisibilityAvailableConfig, V2)) setFeature
    <@> mkNamedAPI @'("get-deprecated", '(ValidateSAMLEmailsConfig, V2)) getFeature
    <@> mkNamedAPI @'("get-deprecated", '(DigitalSignaturesConfig, V2)) getFeature

deprecatedFeatureAPI :: API (AllDeprecatedFeatureConfigAPI DeprecatedFeatureConfigs) GalleyEffects
deprecatedFeatureAPI =
  mkNamedAPI @'("get-config", LegalholdConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", SSOConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", SearchVisibilityAvailableConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", ValidateSAMLEmailsConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", DigitalSignaturesConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", AppLockConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", FileSharingConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", ClassifiedDomainsConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", ConferenceCallingConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", SelfDeletingMessagesConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", GuestLinksConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", SndFactorPasswordChallengeConfig) getSingleFeatureForUser
    <@> mkNamedAPI @'("get-config", MLSConfig) getSingleFeatureForUser
