-- | Swagger utilities.
module Spar.API.Util where

import Imports
import Servant
import Servant.Swagger

-- | A type-level tag that lets us omit any branch from Swagger docs.
data OmitDocs

instance HasSwagger (OmitDocs :> a) where
  toSwagger _ = mempty

instance HasServer api ctx => HasServer (OmitDocs :> api) ctx where
  type ServerT (OmitDocs :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- | A type family that prepares our API for docs generation.
--
-- Our Swagger docs are intended to be used by client developers, and not
-- all endpoints are useful for them or should be seen by them. Here we
-- assume the 'spar' service is only accessible from behind the 'nginz'
-- proxy, which handles authorization (adding a Z-User header if requests
-- are authorized)
--
-- We also omit all branches marked with 'OmitDocs'. Those are likely to be:
--
--   * Endpoints for which we can't generate Swagger docs.
--   * The endpoint that serves Swagger docs.
--   * Internal endpoints.
type family OutsideWorld (api :: k) :: k where
-- special cases
  OutsideWorld (Header "Z-User" a :> b) = OutsideWorld b
  OutsideWorld (OmitDocs :> b) = EmptyAPI
-- recursion
  OutsideWorld (a :<|> b) = OutsideWorld a :<|> OutsideWorld b
  OutsideWorld (a :> b) = a :> OutsideWorld b
  OutsideWorld x = x
