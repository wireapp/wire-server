module Brig.API.Federation where

import Brig.API.Handler (Handler)
import Brig.App (viewFederationDomain)
import qualified Brig.User.Handle as API
import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Qualified (Qualified (Qualified))
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import qualified Wire.API.Federation.API.Brig as FederationAPIBrig

federationSitemap :: ServerT (ToServantApi FederationAPIBrig.Api) Handler
federationSitemap = genericServerT (FederationAPIBrig.Api getUserByHandle)

-- TODO: Write tests
getUserByHandle :: Handle -> Handler (Qualified UserId)
getUserByHandle handle = do
  maybeOwnerId <- lift $ API.lookupHandle handle
  case maybeOwnerId of
    Nothing -> undefined
    Just ownerId -> Qualified ownerId <$> viewFederationDomain
