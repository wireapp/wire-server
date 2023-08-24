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

module Run where

import Cassandra.Schema
import Control.Exception (finally)
import Imports
import System.Logger.Extended qualified as Log
import Util.Options
import V43 qualified
import V44 qualified
import V45 qualified
import V46 qualified
import V47 qualified
import V48 qualified
import V49 qualified
import V50 qualified
import V51 qualified
import V52 qualified
import V53 qualified
import V54 qualified
import V55 qualified
import V56 qualified
import V57 qualified
import V58 qualified
import V59 qualified
import V60_AddFederationIdMapping qualified
import V61_team_invitation_email qualified
import V62_RemoveFederationIdMapping qualified
import V63_AddUsersPendingActivation qualified
import V64_ClientCapabilities qualified
import V65_FederatedConnections qualified
import V66_PersonalFeatureConfCallInit qualified
import V67_MLSKeyPackages qualified
import V68_AddMLSPublicKeys qualified
import V69_MLSKeyPackageRefMapping qualified
import V70_UserEmailUnvalidated qualified
import V71_AddTableVCodesThrottle qualified
import V72_AddNonceTable qualified
import V73_ReplaceNonceTable qualified
import V74_AddOAuthTables qualified
import V75_AddOAuthCodeChallenge qualified
import V76_AddSupportedProtocols qualified
import V77_FederationRemotes qualified
import V78_ClientLastActive qualified
import V79_ConnectionRemoteIndex qualified
import V80_KeyPackageCiphersuite qualified

main :: IO ()
main = do
  let desc = "Brig Cassandra Schema Migrations"
      defaultPath = "/etc/wire/brig/conf/brig-schema.yaml"
  o <- getOptions desc (Just migrationOptsParser) defaultPath
  l <- Log.mkLogger'
  migrateSchema
    l
    o
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
      V80_KeyPackageCiphersuite.migration
      -- When adding migrations here, don't forget to update
      -- 'schemaVersion' in Brig.App

      -- FUTUREWORK: undo V41 (searchable flag); we stopped using it in
      -- https://github.com/wireapp/wire-server/pull/964
      --
      -- FUTUREWORK after July 2023: integrate V_FUTUREWORK here.
    ]
    `finally` Log.close l
