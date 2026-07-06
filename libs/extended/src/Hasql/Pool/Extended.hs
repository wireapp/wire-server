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
    -- | Configured pool acquisition wait time. hasql-resource-pool only
    -- accepts whole seconds here, so we round up to the nearest second.
    acquisitionTimeout :: Duration,
    -- | Kept for config compatibility. hasql-resource-pool does not currently
    -- expose a direct equivalent, so we parse and retain it but do not enforce
    -- it here.
    agingTimeout :: Duration,
    -- | This is the only timeout we actively apply to the resource pool.
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

-- | Creates a pool from postgres config params.
--
-- Only 'idlenessTimeout' is enforced by the new resource pool backend. The
-- other timeout fields stay in the config shape so existing configuration
-- files continue to decode, but they are currently compatibility-only.
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
      poolAcquireSettings
    )
  where
    -- hasql-resource-pool does not expose a direct equivalent for the old
    -- agingTimeout setting. That field remains in the config shape for
    -- compatibility but is not enforced by this pool.
    --
    -- The custom getter above performs the actual connection establishment.
    -- This record only configures pool behavior, including acquisition timing.
    poolAcquireSettings =
      HasqlPool.ConnectionSettings
        { host = "",
          port = 5432,
          user = "",
          password = "",
          dbName = "",
          connAcqTimeout = acquisitionTimeoutSeconds config.acquisitionTimeout,
          txIdleTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          stmtTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          sslMode = "prefer",
          sslRootCert = ""
        }

    acquisitionTimeoutSeconds d
      | d.duration <= 0 = 0
      | otherwise = fromInteger $ ceiling (realToFrac d.duration :: Double)
