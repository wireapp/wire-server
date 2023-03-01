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
import Galley.Cassandra.TeamFeatures
import Imports
import Wire.API.Federation.API
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.Feature
import Wire.API.Team.Feature

featureAPI :: API FeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI @'("get", SSOConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", LegalholdConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", LegalholdConfig) (callsFed (exposeAnnotations (setFeatureStatus @Cassandra . DoAuth)))
    <@> mkNamedAPI @'("get", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put-deprecated", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @"get-search-visibility" getSearchVisibility
    <@> mkNamedAPI @"set-search-visibility" (setSearchVisibility (featureEnabledForTeam @Cassandra @SearchVisibilityAvailableConfig))
    <@> mkNamedAPI @'("get", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get-deprecated", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", AppLockConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", AppLockConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", FileSharingConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", FileSharingConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", ClassifiedDomainsConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", ConferenceCallingConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", SelfDeletingMessagesConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", SelfDeletingMessagesConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", GuestLinksConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", GuestLinksConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", SndFactorPasswordChallengeConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", SndFactorPasswordChallengeConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", MLSConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", MLSConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", ExposeInvitationURLsToTeamAdminConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", SearchVisibilityInboundConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", SearchVisibilityInboundConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", OutlookCalIntegrationConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", OutlookCalIntegrationConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("get", MlsE2EIdConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @'("put", MlsE2EIdConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPI @"get-all-feature-configs-for-user" (getAllFeatureConfigsForUser @Cassandra)
    <@> mkNamedAPI @"get-all-feature-configs-for-team" (getAllFeatureConfigsForTeam @Cassandra)
    <@> mkNamedAPI @'("get-config", LegalholdConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", SSOConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", SearchVisibilityAvailableConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", ValidateSAMLEmailsConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", DigitalSignaturesConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", AppLockConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", FileSharingConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", ClassifiedDomainsConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", ConferenceCallingConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", SelfDeletingMessagesConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", GuestLinksConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", SndFactorPasswordChallengeConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPI @'("get-config", MLSConfig) (getFeatureStatusForUser @Cassandra)
