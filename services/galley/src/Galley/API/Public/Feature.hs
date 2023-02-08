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
    <@> mkNamedAPIWithDomainAndJwk @'("get", LegalholdConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", LegalholdConfig) (callsFed (setFeatureStatus @Cassandra . DoAuth))
    <@> mkNamedAPIWithDomainAndJwk @'("get", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get-deprecated", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put-deprecated", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @"get-search-visibility" getSearchVisibility
    <@> mkNamedAPIWithDomainAndJwk @"set-search-visibility" (setSearchVisibility @Cassandra (featureEnabledForTeam @Cassandra @SearchVisibilityAvailableConfig))
    <@> mkNamedAPIWithDomainAndJwk @'("get", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get-deprecated", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get-deprecated", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", AppLockConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", AppLockConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", FileSharingConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", FileSharingConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", ClassifiedDomainsConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", ConferenceCallingConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", SelfDeletingMessagesConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", SelfDeletingMessagesConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", GuestLinksConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", GuestLinksConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", SndFactorPasswordChallengeConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", SndFactorPasswordChallengeConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", MLSConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", MLSConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", ExposeInvitationURLsToTeamAdminConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", SearchVisibilityInboundConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", SearchVisibilityInboundConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("get", OutlookCalIntegrationConfig) (getFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @'("put", OutlookCalIntegrationConfig) (setFeatureStatus @Cassandra . DoAuth)
    <@> mkNamedAPIWithDomainAndJwk @"get-all-feature-configs-for-user" (getAllFeatureConfigsForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @"get-all-feature-configs-for-team" (getAllFeatureConfigsForTeam @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", LegalholdConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", SSOConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", SearchVisibilityAvailableConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", ValidateSAMLEmailsConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", DigitalSignaturesConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", AppLockConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", FileSharingConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", ClassifiedDomainsConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", ConferenceCallingConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", SelfDeletingMessagesConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", GuestLinksConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", SndFactorPasswordChallengeConfig) (getFeatureStatusForUser @Cassandra)
    <@> mkNamedAPIWithDomainAndJwk @'("get-config", MLSConfig) (getFeatureStatusForUser @Cassandra)
