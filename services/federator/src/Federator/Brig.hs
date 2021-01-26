{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Federator.Brig where

import Control.Lens (view, (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Domain
import Data.Handle
import Data.Id hiding (client)
import Data.Proxy
import Data.String.Conversions
import Federator.App (AppIO)
import Federator.Types
import Imports
import Mu.Server (ServerError (..), ServerErrorCode (..))
import Network.HTTP.Types
import Servant.API
import qualified Servant.API as Servant
import Servant.Client
import qualified System.Logger.Class as Log
import Util.Options
import Wire.API.Federation.GRPC.Proto (QualifiedHandle (..))
import Wire.API.User.Handle

-- FUTUREWORK: can we avoid having to copy this over from Brig.API.Public ?
-- FUTUREWORK: how to make use of servant-client with the odd 'ZAuthServant' type?
type InternalAuth = Header' '[Servant.Required, Servant.Strict] "Z-User" UserId

type GetHandleInfoQualified =
  Summary "Get information on a user handle"
    :> InternalAuth
    :> "users"
    :> "handles"
    :> Capture "domain" Domain
    :> Capture' '[Description "The user handle"] "handle" Handle
    :> Get '[Servant.JSON] UserHandleInfo

api :: Proxy GetHandleInfoQualified
api = Proxy

getUserInfoByHandle :: UserId -> Domain -> Handle -> ClientM UserHandleInfo
getUserInfoByHandle = client api

-- FUTUREWORK: better error handling
-- FUTUREWORK: use a specific /i/something internal brig endpoint to avoid having to "fake" ZAuth user Ids
run :: QualifiedHandle -> AppIO UserHandleInfo
run (QualifiedHandle domain handle) = do
  Log.warn $ Log.msg $ "%%-> Brig/run. Handle:" <> show handle
  manager' <- view httpManager
  b <- view brigEndpoint
  fakeZAuth <- randomId
  let query = getUserInfoByHandle fakeZAuth (Domain domain) (Handle handle) -- FUTUREWORK: validation needs to happen on domain/handle
  res <- liftIO $ runClientM query (mkClientEnv manager' (BaseUrl Http (cs (b ^. epHost)) (fromIntegral (b ^. epPort)) ""))
  case res of
    Left (FailureResponse _ resp) -> do
      case responseStatusCode resp of
        Status 404 _ -> do
          Log.warn $ Log.msg $ "%%-> Brig/run. 404 from brig:" <> show resp
          throwError $ ServerError NotFound ("404 Error: " ++ show resp)
        _ -> undefined
    Left err -> do
      Log.warn $ Log.msg $ "%%-> Brig/run. err from brig: " <> show err
      throwError $ ServerError NotFound ("Error: " ++ show err)
    Right result -> pure $ result

-- FUTUREWORK: round trip tests for
