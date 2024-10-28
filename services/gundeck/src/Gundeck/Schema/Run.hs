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

module Gundeck.Schema.Run where

import Cassandra.MigrateSchema (migrateSchema)
import Cassandra.Schema
import Control.Exception (finally)
import Gundeck.Schema.V1 qualified as V1
import Gundeck.Schema.V10 qualified as V10
import Gundeck.Schema.V11 qualified as V11
import Gundeck.Schema.V12 qualified as V12
import Gundeck.Schema.V2 qualified as V2
import Gundeck.Schema.V3 qualified as V3
import Gundeck.Schema.V4 qualified as V4
import Gundeck.Schema.V5 qualified as V5
import Gundeck.Schema.V6 qualified as V6
import Gundeck.Schema.V7 qualified as V7
import Gundeck.Schema.V8 qualified as V8
import Gundeck.Schema.V9 qualified as V9
import Imports
import System.Logger.Extended qualified as Log
import Util.Options

main :: IO ()
main = do
  o <- getOptions desc (Just migrationOptsParser) defaultPath
  l <- Log.mkLogger'
  migrateSchema
    l
    o
    migrations
    `finally` Log.close l
  where
    desc = "Gundeck Cassandra Schema Migrations"
    defaultPath = "/etc/wire/gundeck/conf/gundeck-schema.yaml"

lastSchemaVersion :: Int32
lastSchemaVersion = migVersion $ last migrations

migrations :: [Migration]
migrations =
  [ V1.migration,
    V2.migration,
    V3.migration,
    V4.migration,
    V5.migration,
    V6.migration,
    V7.migration,
    V8.migration,
    V9.migration,
    V10.migration,
    V11.migration,
    V12.migration
  ]
