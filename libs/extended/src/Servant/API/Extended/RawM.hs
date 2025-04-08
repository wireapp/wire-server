-- | copy of https://github.com/haskell-servant/servant/pull/1551 while we're waiting for this
-- to be released.  this was needed in https://github.com/wireapp/wire-server/pull/2848/, but
-- then in the end it wasn't.  we keep it here in the hope that whoever needs it next will
-- have an easier time putting it to work.
module Servant.API.Extended.RawM where

import Control.Monad.Trans.Resource
import Data.Metrics.Servant
import Data.Proxy
import Imports
import Network.Wai
import Servant.API (Raw)
import Servant.OpenApi
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router

-- TODO: this has been merged to servant in 2023: https://github.com/haskell-servant/servant/pull/1551
type ApplicationM m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

-- | Variant of 'Raw' that lets you access the underlying monadic context to process the request.
data RawM deriving (Typeable)

-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> RawM
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer RawM context where
  type ServerT RawM m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

  route ::
    Proxy RawM ->
    Context context ->
    Delayed env (Request -> (Response -> IO ResponseReceived) -> Handler ResponseReceived) ->
    Router env
  route _ _ handleDelayed = RawRouter $ \env request respond -> runResourceT $ do
    routeResult <- runDelayed handleDelayed env request
    let respond' = liftIO . respond
    liftIO $ case routeResult of
      Route handler ->
        runHandler (handler request (respond . Route))
          >>= \case
            Left e -> respond' $ FailFatal e
            Right a -> pure a
      Fail e -> respond' $ Fail e
      FailFatal e -> respond' $ FailFatal e

  hoistServerWithContext _ _ f srvM req respond = f (srvM req respond)

instance HasOpenApi RawM where
  toOpenApi _ = toOpenApi (Proxy @Raw)

instance RoutesToPaths RawM where
  getRoutes = []
