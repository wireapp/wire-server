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

module Spar.Schema.Run where

import Cassandra.MigrateSchema (migrateSchema)
import Cassandra.Schema
import Control.Exception (finally)
import Imports
import Spar.Schema.V0 qualified as V0
import Spar.Schema.V1 qualified as V1
import Spar.Schema.V10 qualified as V10
import Spar.Schema.V11 qualified as V11
import Spar.Schema.V12 qualified as V12
import Spar.Schema.V13 qualified as V13
import Spar.Schema.V14 qualified as V14
import Spar.Schema.V15 qualified as V15
import Spar.Schema.V16 qualified as V16
import Spar.Schema.V17 qualified as V17
import Spar.Schema.V18 qualified as V18
import Spar.Schema.V19 qualified as V19
import Spar.Schema.V2 qualified as V2
import Spar.Schema.V20 qualified as V20
import Spar.Schema.V21 qualified as V21
import Spar.Schema.V3 qualified as V3
import Spar.Schema.V4 qualified as V4
import Spar.Schema.V5 qualified as V5
import Spar.Schema.V6 qualified as V6
import Spar.Schema.V7 qualified as V7
import Spar.Schema.V8 qualified as V8
import Spar.Schema.V9 qualified as V9
import System.Logger.Extended qualified as Log
import Util.Options

main :: IO ()
main = do
  let desc = "Spar Cassandra Schema Migrations"
      defaultPath = "/etc/wire/spar/conf/spar-schema.yaml"
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
  [ V0.migration,
    V1.migration,
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
    V12.migration,
    V13.migration,
    V14.migration,
    V15.migration,
    V16.migration,
    V17.migration,
    V18.migration,
    V19.migration,
    V20.migration,
    V21.migration
    -- TODO: Add a migration that removes unused fields
    -- (we don't want to risk running a migration which would
    -- effectively break the currently deployed spar service)
    -- see https://github.com/wireapp/wire-server/pull/476.
  ]
