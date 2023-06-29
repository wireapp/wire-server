module Wire.BackgroundWorker.Health where

import qualified Data.Map.Strict as Map
import Imports
import Servant
import Servant.Server.Generic
import Wire.BackgroundWorker.Env

data HealthAPI routes = HealthAPI
  { status :: routes :- "i" :> "status" :> Get '[PlainText] NoContent
  }
  deriving (Generic)

statusImpl :: AppT Handler NoContent
statusImpl = do
  notWorkingWorkers <- Map.keys . Map.filter not <$> (readIORef =<< asks statuses)
  if null notWorkingWorkers
    then pure NoContent
    else lift $ throwError err500 {errBody = "These workers are not working: " <> cs (show notWorkingWorkers)}

api :: Env -> HealthAPI AsServer
api env = fromServant $ hoistServer (Proxy @(ToServant HealthAPI AsApi)) (runAppT env) (toServant apiInAppT)
  where
    apiInAppT :: HealthAPI (AsServerT (AppT Handler))
    apiInAppT = HealthAPI {status = statusImpl}
