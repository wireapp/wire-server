module Wire.API.Routes.NoContentWithStatusCodeVerb where

import Data.Metrics.Servant
import Data.Typeable
import GHC.TypeLits
import Servant.API
import Servant.API.Generic
import Servant.API.Status
import Servant.Server.Internal

data NoContentWithStatusCodeVerb (method :: k1) (statusCode :: Nat)
  deriving (Typeable, Generic)

instance
  (ReflectMethod method, KnownNat status) =>
  HasServer (NoContentWithStatusCodeVerb method status) context
  where
  type ServerT (NoContentWithStatusCodeVerb method status) m = m NoContent
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = noContentRouter method status
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

instance RoutesToPaths (NoContentWithStatusCodeVerb method status) where
  getRoutes = []
