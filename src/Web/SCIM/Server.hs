module Web.SCIM.Server
  (
  -- * WAI application
  app, mkapp, App

  -- * API tree
  , SiteAPI, siteServer
  -- ** API subtrees, useful for tests
  , ConfigAPI, configServer
  , UserAPI, userServer
  , GroupAPI, groupServer
  ) where

import           Web.SCIM.Class.User (UserSite (..), UserDB, userServer)
import           Web.SCIM.Class.Group (GroupSite (..), GroupDB, groupServer)
import           Web.SCIM.Class.Auth (AuthDB (..))
import           Web.SCIM.Capabilities.MetaSchema (ConfigSite, Configuration, configServer)
import           Web.SCIM.Handler
import           GHC.Generics (Generic)
import           Network.Wai
import           Servant.Generic hiding (fromServant)
import           Servant

----------------------------------------------------------------------------
-- API specification

type DB m = (UserDB m, GroupDB m, AuthDB m)

type ConfigAPI        = ToServant (ConfigSite AsApi)
type UserAPI          = ToServant (UserSite AsApi)
type GroupAPI         = ToServant (GroupSite AsApi)
type SiteAPI authData = ToServant (Site authData AsApi)

data Site authData route = Site
  { config :: route :-
      ConfigAPI
  , users :: route :-
      Header "Authorization" authData :>
      "Users" :> UserAPI
  , groups :: route :-
      Header "Authorization" authData :>
      "Groups" :> GroupAPI
  } deriving (Generic)

----------------------------------------------------------------------------
-- API implementation

siteServer ::
  forall m. DB m =>
  Configuration -> Site (AuthData m) (AsServerT (SCIMHandler m))
siteServer conf = Site
  { config = toServant $ configServer conf
  , users = \authData -> toServant (userServer authData)
  , groups = \authData -> toServant (groupServer authData)
  }
  where

----------------------------------------------------------------------------
-- Server-starting utilities

type App m api =
  ( DB m
  , HasServer api '[]
  )

mkapp :: forall m api. (App m api)
      => Proxy api
      -> ServerT api (SCIMHandler m)
      -> (forall a. SCIMHandler m a -> Handler a)
      -> Application
mkapp proxy api nt =
  serve proxy $
    hoistServer proxy nt api

app :: forall m. App m (SiteAPI (AuthData m))
    => Configuration
    -> (forall a. SCIMHandler m a -> Handler a)
    -> Application
app c = mkapp (Proxy @(SiteAPI (AuthData m))) (toServant $ siteServer c)
