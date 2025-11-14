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

module Wire.BackgroundWorker.Health where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Servant
import Servant.Server.Generic
import Wire.BackgroundWorker.Env

data HealthAPI routes = HealthAPI
  { status :: routes :- "i" :> "status" :> Get '[PlainText] NoContent,
    statusWorkers :: routes :- "i" :> "status" :> "workers" :> Get '[PlainText] NoContent
  }
  deriving (Generic)

statusWorkersImpl :: AppT Handler NoContent
statusWorkersImpl = do
  notWorkingWorkers <- Map.keys . Map.filter not <$> (readIORef =<< asks statuses)
  let notWorkingWorkerNames =
        BS.fromStrict
          . Text.encodeUtf8
          . Text.intercalate ", "
          $ map workerName notWorkingWorkers
  if null notWorkingWorkers
    then pure NoContent
    else lift $ throwError err500 {errBody = "These workers are not working: " <> notWorkingWorkerNames}

api :: Env -> HealthAPI AsServer
api env = fromServant $ hoistServer (Proxy @(ToServant HealthAPI AsApi)) (runAppT env) (toServant apiInAppT)
  where
    apiInAppT :: HealthAPI (AsServerT (AppT Handler))
    apiInAppT =
      HealthAPI
        { status = pure NoContent,
          statusWorkers = statusWorkersImpl
        }
