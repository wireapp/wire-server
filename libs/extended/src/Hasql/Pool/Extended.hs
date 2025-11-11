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
import Hasql.Connection.Setting qualified as HasqlSetting
import Hasql.Connection.Setting.Connection qualified as HasqlConn
import Hasql.Connection.Setting.Connection.Param qualified as HasqlConfig
import Hasql.Pool as HasqlPool
import Hasql.Pool.Config qualified as HasqlPool
import Imports
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
--
-- HasqlConn.params translates pgParams into connection (which just holds the connection string and is not a real connection)
-- HasqlSetting.connection unwraps the connection string out of connection
-- HasqlPool.staticConnectionSettings translates the connection string to the pool settings
-- HasqlPool.settings translates the pool settings into pool config
-- HasqlPool.acquire creates the pool.
-- ezpz.
initPostgresPool :: PoolConfig -> Map Text Text -> Maybe FilePathSecrets -> IO HasqlPool.Pool
initPostgresPool config pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  let pgConfigWithPw = maybe pgConfig (\pw -> Map.insert "password" pw pgConfig) mPw
      pgParams = Map.foldMapWithKey (\k v -> [HasqlConfig.other k v]) pgConfigWithPw
  HasqlPool.acquire $
    HasqlPool.settings
      [ HasqlPool.staticConnectionSettings $
          [HasqlSetting.connection $ HasqlConn.params pgParams],
        HasqlPool.size config.size,
        HasqlPool.acquisitionTimeout config.acquisitionTimeout.duration,
        HasqlPool.agingTimeout config.agingTimeout.duration,
        HasqlPool.idlenessTimeout config.idlenessTimeout.duration
      ]
