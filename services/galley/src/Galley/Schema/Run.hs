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

module Galley.Schema.Run where

import Cassandra.MigrateSchema (migrateSchema)
import Cassandra.Schema
import Control.Exception (finally)
import Galley.Schema.V20 qualified as V20
import Galley.Schema.V21 qualified as V21
import Galley.Schema.V22 qualified as V22
import Galley.Schema.V23 qualified as V23
import Galley.Schema.V24 qualified as V24
import Galley.Schema.V25 qualified as V25
import Galley.Schema.V26 qualified as V26
import Galley.Schema.V27 qualified as V27
import Galley.Schema.V28 qualified as V28
import Galley.Schema.V29 qualified as V29
import Galley.Schema.V30 qualified as V30
import Galley.Schema.V31 qualified as V31
import Galley.Schema.V32 qualified as V32
import Galley.Schema.V33 qualified as V33
import Galley.Schema.V34 qualified as V34
import Galley.Schema.V35 qualified as V35
import Galley.Schema.V36 qualified as V36
import Galley.Schema.V37 qualified as V37
import Galley.Schema.V38_CreateTableBillingTeamMember qualified as V38_CreateTableBillingTeamMember
import Galley.Schema.V39 qualified as V39
import Galley.Schema.V40_CreateTableDataMigration qualified as V40_CreateTableDataMigration
import Galley.Schema.V41_TeamNotificationQueue qualified as V41_TeamNotificationQueue
import Galley.Schema.V42_TeamFeatureValidateSamlEmails qualified as V42_TeamFeatureValidateSamlEmails
import Galley.Schema.V43_TeamFeatureDigitalSignatures qualified as V43_TeamFeatureDigitalSignatures
import Galley.Schema.V44_AddRemoteIdentifiers qualified as V44_AddRemoteIdentifiers
import Galley.Schema.V45_AddFederationIdMapping qualified as V45_AddFederationIdMapping
import Galley.Schema.V46_TeamFeatureAppLock qualified as V46_TeamFeatureAppLock
import Galley.Schema.V47_RemoveFederationIdMapping qualified as V47_RemoveFederationIdMapping
import Galley.Schema.V48_DeleteRemoteIdentifiers qualified as V48_DeleteRemoteIdentifiers
import Galley.Schema.V49_ReAddRemoteIdentifiers qualified as V49_ReAddRemoteIdentifiers
import Galley.Schema.V50_AddLegalholdWhitelisted qualified as V50_AddLegalholdWhitelisted
import Galley.Schema.V51_FeatureFileSharing qualified as V51_FeatureFileSharing
import Galley.Schema.V52_FeatureConferenceCalling qualified as V52_FeatureConferenceCalling
import Galley.Schema.V53_AddRemoteConvStatus qualified as V53_AddRemoteConvStatus
import Galley.Schema.V54_TeamFeatureSelfDeletingMessages qualified as V54_TeamFeatureSelfDeletingMessages
import Galley.Schema.V55_SelfDeletingMessagesLockStatus qualified as V55_SelfDeletingMessagesLockStatus
import Galley.Schema.V56_GuestLinksTeamFeatureStatus qualified as V56_GuestLinksTeamFeatureStatus
import Galley.Schema.V57_GuestLinksLockStatus qualified as V57_GuestLinksLockStatus
import Galley.Schema.V58_ConversationAccessRoleV2 qualified as V58_ConversationAccessRoleV2
import Galley.Schema.V59_FileSharingLockStatus qualified as V59_FileSharingLockStatus
import Galley.Schema.V60_TeamFeatureSndFactorPasswordChallenge qualified as V60_TeamFeatureSndFactorPasswordChallenge
import Galley.Schema.V61_MLSConversation qualified as V61_MLSConversation
import Galley.Schema.V62_TeamFeatureSearchVisibilityInbound qualified as V62_TeamFeatureSearchVisibilityInbound
import Galley.Schema.V63_MLSConversationClients qualified as V63_MLSConversationClients
import Galley.Schema.V64_Epoch qualified as V64_Epoch
import Galley.Schema.V65_MLSRemoteClients qualified as V65_MLSRemoteClients
import Galley.Schema.V66_AddSplashScreen qualified as V66_AddSplashScreen
import Galley.Schema.V67_MLSFeature qualified as V67_MLSFeature
import Galley.Schema.V68_MLSCommitLock qualified as V68_MLSCommitLock
import Galley.Schema.V69_MLSProposal qualified as V69_MLSProposal
import Galley.Schema.V70_MLSCipherSuite qualified as V70_MLSCipherSuite
import Galley.Schema.V71_MemberClientKeypackage qualified as V71_MemberClientKeypackage
import Galley.Schema.V72_DropManagedConversations qualified as V72_DropManagedConversations
import Galley.Schema.V73_MemberClientTable qualified as V73_MemberClientTable
import Galley.Schema.V74_ExposeInvitationsToTeamAdmin qualified as V74_ExposeInvitationsToTeamAdmin
import Galley.Schema.V75_MLSGroupInfo qualified as V75_MLSGroupInfo
import Galley.Schema.V76_ProposalOrigin qualified as V76_ProposalOrigin
import Galley.Schema.V77_MLSGroupMemberClient qualified as V77_MLSGroupMemberClient
import Galley.Schema.V78_TeamFeatureOutlookCalIntegration qualified as V78_TeamFeatureOutlookCalIntegration
import Galley.Schema.V79_TeamFeatureMlsE2EId qualified as V79_TeamFeatureMlsE2EId
import Galley.Schema.V80_AddConversationCodePassword qualified as V80_AddConversationCodePassword
import Galley.Schema.V81_TeamFeatureMlsE2EIdUpdate qualified as V81_TeamFeatureMlsE2EIdUpdate
import Galley.Schema.V82_RemoteDomainIndexes qualified as V82_RemoteDomainIndexes
import Galley.Schema.V83_CreateTableTeamAdmin qualified as V83_CreateTableTeamAdmin
import Galley.Schema.V84_MLSSubconversation qualified as V84_MLSSubconversation
import Galley.Schema.V85_MLSDraft17 qualified as V85_MLSDraft17
import Galley.Schema.V86_TeamFeatureMlsMigration qualified as V86_TeamFeatureMlsMigration
import Galley.Schema.V87_TeamFeatureSupportedProtocols qualified as V87_TeamFeatureSupportedProtocols
import Galley.Schema.V88_RemoveMemberClientAndTruncateMLSGroupMemberClient qualified as V88_RemoveMemberClientAndTruncateMLSGroupMemberClient
import Galley.Schema.V89_MlsLockStatus qualified as V89_MlsLockStatus
import Galley.Schema.V90_EnforceFileDownloadLocationConfig qualified as V90_EnforceFileDownloadLocationConfig
import Galley.Schema.V91_TeamMemberDeletedLimitedEventFanout qualified as V91_TeamMemberDeletedLimitedEventFanout
import Galley.Schema.V92_MlsE2EIdConfig qualified as V92_MlsE2EIdConfig
import Galley.Schema.V93_ConferenceCallingSftForOneToOne qualified as V93_ConferenceCallingSftForOneToOne
import Galley.Schema.V94_DomainRegistrationConfig qualified as V94_DomainRegistrationConfig
import Galley.Schema.V95_DynamicFeatures qualified as V95_DynamicFeatures
import Imports
import Options.Applicative
import System.Logger.Extended qualified as Log

main :: IO ()
main = do
  o <- execParser (info (helper <*> migrationOptsParser) desc)
  l <- Log.mkLogger'
  migrateSchema
    l
    o
    migrations
    `finally` Log.close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc

lastSchemaVersion :: Int32
lastSchemaVersion = migVersion $ last migrations

migrations :: [Migration]
migrations =
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
    V83_CreateTableTeamAdmin.migration,
    V84_MLSSubconversation.migration,
    V85_MLSDraft17.migration,
    V86_TeamFeatureMlsMigration.migration,
    V87_TeamFeatureSupportedProtocols.migration,
    V88_RemoveMemberClientAndTruncateMLSGroupMemberClient.migration,
    V89_MlsLockStatus.migration,
    V90_EnforceFileDownloadLocationConfig.migration,
    V91_TeamMemberDeletedLimitedEventFanout.migration,
    V92_MlsE2EIdConfig.migration,
    V93_ConferenceCallingSftForOneToOne.migration,
    V94_DomainRegistrationConfig.migration,
    V95_DynamicFeatures.migration
    -- FUTUREWORK: once #1726 has made its way to master/production,
    -- the 'message' field in connections table can be dropped.
    -- See also https://github.com/wireapp/wire-server/pull/1747/files
    -- for an explanation
    -- FUTUREWORK: once #1751 has made its way to master/production,
    -- the 'otr_muted' field in the member table can be dropped.
  ]
