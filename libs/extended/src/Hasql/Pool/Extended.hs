-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Hasql.Pool.Extended where

import Data.Aeson
import Data.Map as Map
import Data.Misc
import Hasql.Connection qualified
import Hasql.Connection.Settings qualified as HasqlConnSettings
import Hasql.Pool as HasqlPool
import Imports
import PostgresqlConnectionString qualified
import Util.Options

data PoolConfig = PoolConfig
  { size :: Int,
    acquisitionTimeout :: Duration,
    agingTimeout :: Duration,
    idlenessTimeout :: Duration
  }
  deriving (Eq, Show)

instance FromJSON PoolConfig where
  parseJSON = withObject "PoolConfig" $ \o ->
    PoolConfig
      <$> o .: "size"
      <*> o .: "acquisitionTimeout"
      <*> o .: "agingTimeout"
      <*> o .: "idlenessTimeout"

-- | Creates a pool from postgres config params
initPostgresPool :: PoolConfig -> Map Text Text -> Maybe FilePathSecrets -> IO HasqlPool.Pool
initPostgresPool config pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  let pgSettings =
        HasqlConnSettings.connectionString (PostgresqlConnectionString.toUrl $ PostgresqlConnectionString.fromKeyValueParams pgConfig)
          <> foldMap HasqlConnSettings.password mPw
  HasqlPool.acquireWith
    (Hasql.Connection.acquire pgSettings)
    ( config.size,
      realToFrac config.idlenessTimeout.duration,
      unusedConnectionSettings
    )
  where
    -- hasql-resource-pool does not expose equivalents for the old
    -- acquisitionTimeout and agingTimeout settings. Those fields remain in the
    -- config shape for compatibility but are not enforced by this pool.
    --
    -- The custom connection getter above keeps the existing wire-server
    -- connection-string parsing path. hasql-resource-pool ignores this
    -- settings value when acquireWith is used.
    unusedConnectionSettings =
      HasqlPool.ConnectionSettings
        { host = "",
          port = 5432,
          user = "",
          password = "",
          dbName = "",
          connAcqTimeout = 0,
          txIdleTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          stmtTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          sslMode = "prefer",
          sslRootCert = ""
        }
