module Wire.AWS where

import Amazonka (Env, runResourceT)
import Amazonka.Core.Lens.Internal qualified as AWS
import Amazonka.Send as AWS
import Amazonka.Types qualified as AWS
import Control.Lens
import Imports
import Network.HTTP.Client
import Polysemy
import Polysemy.Input

sendCatch :: (Member (Input Amazonka.Env) r, Member (Embed IO) r, AWS.AWSRequest req) => req -> Sem r (Either AWS.Error (AWS.AWSResponse req))
sendCatch req = do
  env <- input
  embed . AWS.trying AWS._Error . runResourceT . AWS.send env $ req

canRetry :: Either AWS.Error a -> Bool
canRetry (Right _) = False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> True
  AWS.ServiceError se | se ^. AWS.serviceError_code == AWS.ErrorCode "RequestThrottled" -> True
  _ -> False
