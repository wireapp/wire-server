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
import qualified V10
import qualified V11
import qualified V12
import qualified V13
import qualified V14
import qualified V15
import qualified V16
import qualified V17
import qualified V18
import qualified V19
import qualified V20
import qualified V21
import qualified V22
import qualified V23
import qualified V24
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
import qualified V38
import qualified V39
import qualified V40
import qualified V41
import qualified V42
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
import qualified V9

main :: IO ()
main = do
  let desc = "Brig Cassandra Schema Migrations"
      defaultPath = "/etc/wire/brig/conf/brig-schema.yaml"
  o <- getOptions desc (Just migrationOptsParser) defaultPath
  l <- Log.mkLogger'
  migrateSchema
    l
    o
    [ V9.migration,
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
      V21.migration,
      V22.migration,
      V23.migration,
      V24.migration,
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
      V38.migration,
      V39.migration,
      V40.migration,
      V41.migration,
      V42.migration,
      V43.migration,
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
      V61_team_invitation_email.migration
      -- FUTUREWORK: undo V41 (searchable flag); we stopped using it in
      -- https://github.com/wireapp/wire-server/pull/964
    ]
    `finally` Log.close l
