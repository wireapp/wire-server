-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import Options.Applicative
import qualified System.Logger.Extended as Log
import qualified V20
import qualified V21
import qualified V22
import qualified V23
import qualified V24
import qualified V25
import qualified V26
import qualified V27
import qualified V28
import qualified V29
import qualified V30
import qualified V31
import qualified V32
import qualified V33
import qualified V34
import qualified V35
import qualified V36
import qualified V37
import qualified V38_CreateTableBillingTeamMember
import qualified V39
import qualified V40_CreateTableDataMigration
import qualified V41_TeamNotificationQueue
import qualified V42_TeamFeatureValidateSamlEmails
import qualified V43_TeamFeatureDigitalSignatures
import qualified V44_AddRemoteIdentifiers
import qualified V45_AddFederationIdMapping
import qualified V46_TeamFeatureAppLock
import qualified V47_RemoveFederationIdMapping
import qualified V48_DeleteRemoteIdentifiers
import qualified V49_ReAddRemoteIdentifiers
import qualified V50_AddLegalholdWhitelisted
import qualified V51_FeatureFileSharing
import qualified V52_FeatureConferenceCalling
import qualified V53_AddRemoteConvStatus

main :: IO ()
main = do
  o <- execParser (info (helper <*> migrationOptsParser) desc)
  l <- Log.mkLogger'
  migrateSchema
    l
    o
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
      V53_AddRemoteConvStatus.migration
      -- When adding migrations here, don't forget to update
      -- 'schemaVersion' in Galley.Data
      -- (see also docs/developer/cassandra-interaction.md)
      --
      -- FUTUREWORK: once #1726 has made its way to master/production,
      -- the 'message' field in connections table can be dropped.
      -- See also https://github.com/wireapp/wire-server/pull/1747/files
      -- for an explanation
      -- FUTUREWORK: once #1751 has made its way to master/production,
      -- the 'otr_muted' field in the member table can be dropped.
    ]
    `finally` Log.close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc
