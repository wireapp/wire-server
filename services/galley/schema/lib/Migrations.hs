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

module Migrations where

import Cassandra.Schema
import Imports
import V20 qualified
import V21 qualified
import V22 qualified
import V23 qualified
import V24 qualified
import V25 qualified
import V26 qualified
import V27 qualified
import V28 qualified
import V29 qualified
import V30 qualified
import V31 qualified
import V32 qualified
import V33 qualified
import V34 qualified
import V35 qualified
import V36 qualified
import V37 qualified
import V38_CreateTableBillingTeamMember qualified
import V39 qualified
import V40_CreateTableDataMigration qualified
import V41_TeamNotificationQueue qualified
import V42_TeamFeatureValidateSamlEmails qualified
import V43_TeamFeatureDigitalSignatures qualified
import V44_AddRemoteIdentifiers qualified
import V45_AddFederationIdMapping qualified
import V46_TeamFeatureAppLock qualified
import V47_RemoveFederationIdMapping qualified
import V48_DeleteRemoteIdentifiers qualified
import V49_ReAddRemoteIdentifiers qualified
import V50_AddLegalholdWhitelisted qualified
import V51_FeatureFileSharing qualified
import V52_FeatureConferenceCalling qualified
import V53_AddRemoteConvStatus qualified
import V54_TeamFeatureSelfDeletingMessages qualified
import V55_SelfDeletingMessagesLockStatus qualified
import V56_GuestLinksTeamFeatureStatus qualified
import V57_GuestLinksLockStatus qualified
import V58_ConversationAccessRoleV2 qualified
import V59_FileSharingLockStatus qualified
import V60_TeamFeatureSndFactorPasswordChallenge qualified
import V61_MLSConversation qualified
import V62_TeamFeatureSearchVisibilityInbound qualified
import V63_MLSConversationClients qualified
import V64_Epoch qualified
import V65_MLSRemoteClients qualified
import V66_AddSplashScreen qualified
import V67_MLSFeature qualified
import V68_MLSCommitLock qualified
import V69_MLSProposal qualified
import V70_MLSCipherSuite qualified
import V71_MemberClientKeypackage qualified
import V72_DropManagedConversations qualified
import V73_MemberClientTable qualified
import V74_ExposeInvitationsToTeamAdmin qualified
import V75_MLSGroupInfo qualified
import V76_ProposalOrigin qualified
import V77_MLSGroupMemberClient qualified
import V78_TeamFeatureOutlookCalIntegration qualified
import V79_TeamFeatureMlsE2EId qualified
import V80_AddConversationCodePassword qualified
import V81_TeamFeatureMlsE2EIdUpdate qualified
import V82_RemoteDomainIndexes qualified
import V83_CreateTableTeamAdmin qualified

lastSchemaVersion :: Int32
lastSchemaVersion = migVersion $ last migrationList

migrationList :: [Migration]
migrationList =
  [ V20.migration,
    V21.migration,
    V22.migration,
    V23.migration,
    V24.migration,
    V25.migration,
    V26.migration,
    V27.migration,
    V28.migration,
    V29.migration,
    V30.migration,
    V31.migration,
    V32.migration,
    V33.migration,
    V34.migration,
    V35.migration,
    V36.migration,
    V37.migration,
    V38_CreateTableBillingTeamMember.migration,
    V39.migration,
    V40_CreateTableDataMigration.migration,
    V41_TeamNotificationQueue.migration,
    V42_TeamFeatureValidateSamlEmails.migration,
    V43_TeamFeatureDigitalSignatures.migration,
    V44_AddRemoteIdentifiers.migration,
    V45_AddFederationIdMapping.migration,
    V46_TeamFeatureAppLock.migration,
    V47_RemoveFederationIdMapping.migration,
    V48_DeleteRemoteIdentifiers.migration,
    V49_ReAddRemoteIdentifiers.migration,
    V50_AddLegalholdWhitelisted.migration,
    V51_FeatureFileSharing.migration,
    V52_FeatureConferenceCalling.migration,
    V53_AddRemoteConvStatus.migration,
    V54_TeamFeatureSelfDeletingMessages.migration,
    V55_SelfDeletingMessagesLockStatus.migration,
    V56_GuestLinksTeamFeatureStatus.migration,
    V57_GuestLinksLockStatus.migration,
    V58_ConversationAccessRoleV2.migration,
    V59_FileSharingLockStatus.migration,
    V60_TeamFeatureSndFactorPasswordChallenge.migration,
    V61_MLSConversation.migration,
    V62_TeamFeatureSearchVisibilityInbound.migration,
    V63_MLSConversationClients.migration,
    V64_Epoch.migration,
    V65_MLSRemoteClients.migration,
    V66_AddSplashScreen.migration,
    V67_MLSFeature.migration,
    V68_MLSCommitLock.migration,
    V69_MLSProposal.migration,
    V70_MLSCipherSuite.migration,
    V71_MemberClientKeypackage.migration,
    V72_DropManagedConversations.migration,
    V73_MemberClientTable.migration,
    V74_ExposeInvitationsToTeamAdmin.migration,
    V75_MLSGroupInfo.migration,
    V76_ProposalOrigin.migration,
    V77_MLSGroupMemberClient.migration,
    V78_TeamFeatureOutlookCalIntegration.migration,
    V79_TeamFeatureMlsE2EId.migration,
    V80_AddConversationCodePassword.migration,
    V81_TeamFeatureMlsE2EIdUpdate.migration,
    V82_RemoteDomainIndexes.migration,
    V83_CreateTableTeamAdmin.migration
    -- When adding migrations here, don't forget to update
    -- 'schemaVersion' in Galley.Cassandra
    -- (see also docs/developer/cassandra-interaction.md)
    --
    -- FUTUREWORK: once #1726 has made its way to master/production,
    -- the 'message' field in connections table can be dropped.
    -- See also https://github.com/wireapp/wire-server/pull/1747/files
    -- for an explanation
    -- FUTUREWORK: once #1751 has made its way to master/production,
    -- the 'otr_muted' field in the member table can be dropped.
  ]
