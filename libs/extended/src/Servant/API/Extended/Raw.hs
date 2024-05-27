module Servant.API.Extended.Raw where

import Control.Monad.Trans.Resource
import Data.Metrics.Servant
import Imports
import Network.Wai qualified as Wai
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router

-- | Provides access to the 'Wai.Request'
data RawRequest

instance RoutesToPaths api => RoutesToPaths (RawRequest :> api) where
  getRoutes = getRoutes @api

instance HasServer api context => HasServer (RawRequest :> api) context where
  type ServerT (RawRequest :> api) m = Wai.Request -> ServerT api m

  route :: Proxy (RawRequest :> api) -> Context context -> Delayed env (Server (RawRequest :> api)) -> Router env
  route _ context subserver = route (Proxy @api) context (passToServer subserver id)
  hoistServerWithContext :: Proxy (RawRequest :> api) -> Proxy context -> (forall x. m x -> n x) -> ServerT (RawRequest :> api) m -> ServerT (RawRequest :> api) n
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- | Allows responding with 'Wai.Response' in any monad. This is better compared
-- to 'Raw' because:
--
-- 1. 'Raw' forces use to the Wai continuation of type
-- '(Response -> IO ResponseRecieved)', which forces use of 'IO' directly in the
-- application code.
--
-- 2. There are no continuations in the application code, so it is simpler to
-- read/write.
data RawResponse

instance RoutesToPaths RawResponse where
  getRoutes = mempty

instance HasServer RawResponse context where
  type ServerT RawResponse m = m Wai.Response

  route :: Proxy RawResponse -> Context context -> Delayed env (Server RawResponse) -> Router env
  route _ _ rawApplication = RawRouter $ \env request cont -> runResourceT $ do
    r <- runDelayed rawApplication env request
    liftIO $
      runHandler (sequenceRouteResult r) >>= \case
        Left e -> cont $ Fail e
        Right rr -> cont rr

  hoistServerWithContext :: Proxy RawResponse -> Proxy context -> (forall x. m x -> n x) -> ServerT RawResponse m -> ServerT RawResponse n
  hoistServerWithContext _ _ nt x = nt x

-- | 'RouteResult' doesn't have a 'Traversable' instance
sequenceRouteResult :: Monad m => RouteResult (m a) -> m (RouteResult a)
sequenceRouteResult (Fail e) = pure $ Fail e
sequenceRouteResult (FailFatal e) = pure $ FailFatal e
sequenceRouteResult (Route x) = Route <$> x
