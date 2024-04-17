module Wire.BackgroundWorker.Health where

import Data.ByteString.Lazy.UTF8 qualified as UTF8
import Data.Map.Strict qualified as Map
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
  if null notWorkingWorkers
    then pure NoContent
    else lift $ throwError err500 {errBody = "These workers are not working: " <> UTF8.fromString (show notWorkingWorkers)}

api :: Env -> HealthAPI AsServer
api env = fromServant $ hoistServer (Proxy @(ToServant HealthAPI AsApi)) (runAppT env) (toServant apiInAppT)
  where
    apiInAppT :: HealthAPI (AsServerT (AppT Handler))
    apiInAppT =
      HealthAPI
        { status = pure NoContent,
          statusWorkers = statusWorkersImpl
        }
