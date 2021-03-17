{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Spar.DataMigration.Types where

import qualified Cassandra as C
import Control.Lens
import Imports
import Numeric.Natural (Natural)
import qualified System.Logger as Logger

data Migration = Migration
  { version :: MigrationVersion,
    text :: Text,
    action :: Env -> IO ()
  }

newtype MigrationVersion = MigrationVersion {migrationVersion :: Natural}
  deriving (Show, Eq, Ord)

data Env = Env
  { sparCassandra :: C.ClientState,
    brigCassandra :: C.ClientState,
    logger :: Logger.Logger,
    pageSize :: Int32,
    debug :: Debug,
    dryRun :: DryRun
  }

data Debug = Debug | NoDebug
  deriving (Show, Eq)

data DryRun = DryRun | NoDryRun
  deriving (Show, Eq)

data MigratorSettings = MigratorSettings
  { _setCasSpar :: !CassandraSettings,
    _setCasBrig :: !CassandraSettings,
    _setDebug :: Debug,
    _setDryRun :: DryRun,
    _setPageSize :: Int32
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { _cHosts :: !String,
    _cPort :: !Word16,
    _cKeyspace :: !C.Keyspace
  }
  deriving (Show)

makeLenses ''MigratorSettings

makeLenses ''CassandraSettings
