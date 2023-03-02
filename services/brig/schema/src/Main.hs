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

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Imports
import qualified System.Logger.Extended as Log
import Util.Options
import qualified V43
import qualified V44
import qualified V45
import qualified V46
import qualified V47
import qualified V48
import qualified V49
import qualified V50
import qualified V51
import qualified V52
import qualified V53
import qualified V54
import qualified V55
import qualified V56
import qualified V57
import qualified V58
import qualified V59
import qualified V60_AddFederationIdMapping
import qualified V61_team_invitation_email
import qualified V62_RemoveFederationIdMapping
import qualified V63_AddUsersPendingActivation
import qualified V64_ClientCapabilities
import qualified V65_FederatedConnections
import qualified V66_PersonalFeatureConfCallInit
import qualified V67_MLSKeyPackages
import qualified V68_AddMLSPublicKeys
import qualified V69_MLSKeyPackageRefMapping
import qualified V70_UserEmailUnvalidated
import qualified V71_AddTableVCodesThrottle
import qualified V72_AddNonceTable
import qualified V73_ReplaceNonceTable
import qualified V74_AddOAuthTables

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
      V74_AddOAuthTables.migration
      -- When adding migrations here, don't forget to update
      -- 'schemaVersion' in Brig.App

      -- FUTUREWORK: undo V41 (searchable flag); we stopped using it in
      -- https://github.com/wireapp/wire-server/pull/964
      --
      -- FUTUREWORK after July 2023: integrate V_FUTUREWORK here.
    ]
    `finally` Log.close l
