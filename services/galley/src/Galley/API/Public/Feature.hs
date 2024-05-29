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
import Galley.App
import Imports
import Wire.API.Federation.API
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Feature
import Wire.API.Team.Feature

featureAPI :: API FeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI @'("get", SSOConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", LegalholdConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", LegalholdConfig) (callsFed (exposeAnnotations (setFeatureStatus . DoAuth)))
    <@> mkNamedAPI @'("get", SearchVisibilityAvailableConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", SearchVisibilityAvailableConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", SearchVisibilityAvailableConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put-deprecated", SearchVisibilityAvailableConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @"get-search-visibility" getSearchVisibility
    <@> mkNamedAPI @"set-search-visibility" (setSearchVisibility (featureEnabledForTeam @SearchVisibilityAvailableConfig))
    <@> mkNamedAPI @'("get", ValidateSAMLEmailsConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", ValidateSAMLEmailsConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", DigitalSignaturesConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", DigitalSignaturesConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", AppLockConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", AppLockConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", FileSharingConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", FileSharingConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", ClassifiedDomainsConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", ConferenceCallingConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", SelfDeletingMessagesConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", SelfDeletingMessagesConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", GuestLinksConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", GuestLinksConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", SndFactorPasswordChallengeConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", SndFactorPasswordChallengeConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", MLSConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", MLSConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", ExposeInvitationURLsToTeamAdminConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", SearchVisibilityInboundConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", SearchVisibilityInboundConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", OutlookCalIntegrationConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", OutlookCalIntegrationConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", MlsE2EIdConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @"put-MlsE2EIdConfig@v5" (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", MlsE2EIdConfig) (guardMlsE2EIdConfig (setFeatureStatus . DoAuth))
    <@> mkNamedAPI @'("get", MlsMigrationConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", MlsMigrationConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", EnforceFileDownloadLocationConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("put", EnforceFileDownloadLocationConfig) (setFeatureStatus . DoAuth)
    <@> mkNamedAPI @'("get", LimitedEventFanoutConfig) (getFeatureStatus . DoAuth)
    <@> mkNamedAPI @"get-all-feature-configs-for-user" getAllFeatureConfigsForUser
    <@> mkNamedAPI @"get-all-feature-configs-for-team" getAllFeatureConfigsForTeam
    <@> mkNamedAPI @'("get-config", LegalholdConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", SSOConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", SearchVisibilityAvailableConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", ValidateSAMLEmailsConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", DigitalSignaturesConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", AppLockConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", FileSharingConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", ClassifiedDomainsConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", ConferenceCallingConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", SelfDeletingMessagesConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", GuestLinksConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", SndFactorPasswordChallengeConfig) getFeatureStatusForUser
    <@> mkNamedAPI @'("get-config", MLSConfig) getFeatureStatusForUser
