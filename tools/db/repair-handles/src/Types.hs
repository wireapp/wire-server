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

module Types where

import Brig.Data.Instances ()
import Cassandra hiding (Set)
import Control.Lens
import Data.Id
import Imports
import qualified System.Logger as Log

data Env = Env
  { envBrig :: ClientState,
    envGalley :: ClientState,
    envPageSize :: Int32,
    envTeam :: TeamId,
    envSettings :: Settings,
    envLogger :: Log.Logger
  }

data Settings = Settings
  { _setCasBrig :: !CassandraSettings,
    _setCasGalley :: !CassandraSettings,
    _setDryRun :: Bool,
    _setDebug :: Bool,
    _setPageSize :: Int32,
    _setTeamId :: TeamId
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { _cHosts :: !String,
    _cPort :: !Word16,
    _cKeyspace :: !Keyspace
  }
  deriving (Show)

makeLenses ''Settings

makeLenses ''CassandraSettings
