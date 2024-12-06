-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Schema.Run where

import Brig.Schema.V43 qualified as V43
import Brig.Schema.V44 qualified as V44
import Brig.Schema.V45 qualified as V45
import Brig.Schema.V46 qualified as V46
import Brig.Schema.V47 qualified as V47
import Brig.Schema.V48 qualified as V48
import Brig.Schema.V49 qualified as V49
import Brig.Schema.V50 qualified as V50
import Brig.Schema.V51 qualified as V51
import Brig.Schema.V52 qualified as V52
import Brig.Schema.V53 qualified as V53
import Brig.Schema.V54 qualified as V54
import Brig.Schema.V55 qualified as V55
import Brig.Schema.V56 qualified as V56
import Brig.Schema.V57 qualified as V57
import Brig.Schema.V58 qualified as V58
import Brig.Schema.V59 qualified as V59
import Brig.Schema.V60_AddFederationIdMapping qualified as V60_AddFederationIdMapping
import Brig.Schema.V61_team_invitation_email qualified as V61_team_invitation_email
import Brig.Schema.V62_RemoveFederationIdMapping qualified as V62_RemoveFederationIdMapping
import Brig.Schema.V63_AddUsersPendingActivation qualified as V63_AddUsersPendingActivation
import Brig.Schema.V64_ClientCapabilities qualified as V64_ClientCapabilities
import Brig.Schema.V65_FederatedConnections qualified as V65_FederatedConnections
import Brig.Schema.V66_PersonalFeatureConfCallInit qualified as V66_PersonalFeatureConfCallInit
import Brig.Schema.V67_MLSKeyPackages qualified as V67_MLSKeyPackages
import Brig.Schema.V68_AddMLSPublicKeys qualified as V68_AddMLSPublicKeys
import Brig.Schema.V69_MLSKeyPackageRefMapping qualified as V69_MLSKeyPackageRefMapping
import Brig.Schema.V70_UserEmailUnvalidated qualified as V70_UserEmailUnvalidated
import Brig.Schema.V71_AddTableVCodesThrottle qualified as V71_AddTableVCodesThrottle
import Brig.Schema.V72_AddNonceTable qualified as V72_AddNonceTable
import Brig.Schema.V73_ReplaceNonceTable qualified as V73_ReplaceNonceTable
import Brig.Schema.V74_AddOAuthTables qualified as V74_AddOAuthTables
import Brig.Schema.V75_AddOAuthCodeChallenge qualified as V75_AddOAuthCodeChallenge
import Brig.Schema.V76_AddSupportedProtocols qualified as V76_AddSupportedProtocols
import Brig.Schema.V77_FederationRemotes qualified as V77_FederationRemotes
import Brig.Schema.V78_ClientLastActive qualified as V78_ClientLastActive
import Brig.Schema.V79_ConnectionRemoteIndex qualified as V79_ConnectionRemoteIndex
import Brig.Schema.V80_KeyPackageCiphersuite qualified as V80_KeyPackageCiphersuite
import Brig.Schema.V81_AddFederationRemoteTeams qualified as V81_AddFederationRemoteTeams
import Brig.Schema.V82_DropPhoneColumn qualified as V82_DropPhoneColumn
import Brig.Schema.V83_AddTextStatus qualified as V83_AddTextStatus
import Brig.Schema.V84_DropTeamInvitationPhone qualified as V84_DropTeamInvitationPhone
import Brig.Schema.V85_DropUserKeysHashed qualified as V85_DropUserKeysHashed
import Brig.Schema.V86_WriteTimeBumper qualified as V86_WriteTimeBumper
import Brig.Schema.V87_DropInvitationTables qualified as V87_DropInvitationTables
import Brig.Schema.V88_DomainRegistrationTable qualified as V88_DomainRegistrationTable
import Cassandra.MigrateSchema (migrateSchema)
import Cassandra.Schema
import Control.Exception (finally)
import Imports
import System.Logger.Extended qualified as Log
import Util.Options

main :: IO ()
main = do
  let desc = "Brig Cassandra Schema Migrations"
      defaultPath = "/etc/wire/brig/conf/brig-schema.yaml"
  o <- getOptions desc (Just migrationOptsParser) defaultPath
  l <- Log.mkLogger'
  migrateSchema
    l
    o
    migrations
    `finally` Log.close l

lastSchemaVersion :: Int32
lastSchemaVersion = migVersion $ last migrations

migrations :: [Migration]
migrations =
  [ V43.migration,
    V44.migration,
    V45.migration,
    V46.migration,
    V47.migration,
    V48.migration,
    V49.migration,
    V50.migration,
    V51.migration,
    V52.migration,
    V53.migration,
    V54.migration,
    V55.migration,
    V56.migration,
    V57.migration,
    V58.migration,
    V59.migration,
    V60_AddFederationIdMapping.migration,
    V61_team_invitation_email.migration,
    V62_RemoveFederationIdMapping.migration,
    V63_AddUsersPendingActivation.migration,
    V64_ClientCapabilities.migration,
    V65_FederatedConnections.migration,
    V66_PersonalFeatureConfCallInit.migration,
    V67_MLSKeyPackages.migration,
    V68_AddMLSPublicKeys.migration,
    V69_MLSKeyPackageRefMapping.migration,
    V70_UserEmailUnvalidated.migration,
    V71_AddTableVCodesThrottle.migration,
    V72_AddNonceTable.migration,
    V73_ReplaceNonceTable.migration,
    V74_AddOAuthTables.migration,
    V75_AddOAuthCodeChallenge.migration,
    V76_AddSupportedProtocols.migration,
    V77_FederationRemotes.migration,
    V78_ClientLastActive.migration,
    V79_ConnectionRemoteIndex.migration,
    V80_KeyPackageCiphersuite.migration,
    V81_AddFederationRemoteTeams.migration,
    V82_DropPhoneColumn.migration,
    V83_AddTextStatus.migration,
    V84_DropTeamInvitationPhone.migration,
    V85_DropUserKeysHashed.migration,
    V86_WriteTimeBumper.migration,
    V87_DropInvitationTables.migration,
    V88_DomainRegistrationTable.migration
    -- FUTUREWORK: undo V41 (searchable flag); we stopped using it in
    -- https://github.com/wireapp/wire-server/pull/964
  ]
