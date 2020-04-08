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
import qualified System.Logger.Extended as Log
import Util.Options
import qualified V1
import qualified V2
import qualified V3
import qualified V4
import qualified V5
import qualified V6
import qualified V7

main :: IO ()
main = do
  o <- getOptions desc (Just migrationOptsParser) defaultPath
  l <- Log.mkLogger'
  migrateSchema
    l
    o
    [ V1.migration,
      V2.migration,
      V3.migration,
      V4.migration,
      V5.migration,
      V6.migration,
      V7.migration
    ]
    `finally` Log.close l
  where
    desc = "Gundeck Cassandra Schema Migrations"
    defaultPath = "/etc/wire/gundeck/conf/gundeck-schema.yaml"
