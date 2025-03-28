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
import qualified Spar.Schema.V0 as V0
import qualified Spar.Schema.V1 as V1
import qualified Spar.Schema.V10 as V10
import qualified Spar.Schema.V11 as V11
import qualified Spar.Schema.V12 as V12
import qualified Spar.Schema.V13 as V13
import qualified Spar.Schema.V14 as V14
import qualified Spar.Schema.V15 as V15
import qualified Spar.Schema.V16 as V16
import qualified Spar.Schema.V17 as V17
import qualified Spar.Schema.V18 as V18
import qualified Spar.Schema.V19 as V19
import qualified Spar.Schema.V2 as V2
import qualified Spar.Schema.V20 as V20
import qualified Spar.Schema.V3 as V3
import qualified Spar.Schema.V4 as V4
import qualified Spar.Schema.V5 as V5
import qualified Spar.Schema.V6 as V6
import qualified Spar.Schema.V7 as V7
import qualified Spar.Schema.V8 as V8
import qualified Spar.Schema.V9 as V9
import qualified System.Logger.Extended as Log
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
    V20.migration
    -- TODO: Add a migration that removes unused fields
    -- (we don't want to risk running a migration which would
    -- effectively break the currently deployed spar service)
    -- see https://github.com/wireapp/wire-server/pull/476.
  ]
