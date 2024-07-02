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

module Run where

import Brig.DataMigration
import Imports
import Options.Applicative
import System.Logger.Extended qualified as Log
import V1_DropPhone qualified

main :: IO ()
main = do
  o <- execParser (info (helper <*> cassandraSettingsParser) desc)
  l <- Log.mkLogger Log.Debug Nothing Nothing
  migrate
    l
    o
    [ V1_DropPhone.migration
    ]
  where
    desc = header "Brig Cassandra Data Migrations" <> fullDesc
