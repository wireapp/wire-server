{-# LANGUAGE CPP #-}

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
import           Web.SCIM.Class.Auth (AuthDB (..), SCIMAuthData (..))
import           Web.SCIM.Capabilities.MetaSchema (ConfigSite, Configuration, configServer)
import           Web.SCIM.Handler
import           Web.SCIM.Schema.Error
import           GHC.Generics (Generic)
import           Data.Text
import           Network.Wai
import           Servant.Generic hiding (fromServant)
#if MIN_VERSION_servant_server(0,12,0)
import           Servant hiding (hoistServer)
import qualified Servant
#else
import           Servant
import           Servant.Utils.Enter
#endif

----------------------------------------------------------------------------
-- API specification

type DB m = (UserDB m, GroupDB m, AuthDB m)

type ConfigAPI = ToServant (ConfigSite AsApi)
type UserAPI   = ToServant (UserSite AsApi)
type GroupAPI  = ToServant (GroupSite AsApi)
type SiteAPI   = ToServant (Site AsApi)

data Site route = Site
  { config :: route :-
      ConfigAPI
  , users :: route :-
      Header "Authorization" SCIMAuthData :>
      "Users" :> UserAPI
  , groups :: route :-
      Header "Authorization" SCIMAuthData :>
      "Groups" :> GroupAPI
  } deriving (Generic)

----------------------------------------------------------------------------
-- Compatibility

-- TODO: this is horrible, let's switch to servant-server-0.12 as soon as possible.  if that has
-- happened, simply remove the CPP language extension and clean up after the errors.

#if MIN_VERSION_servant_server(0,12,0)
type EnterBoilerplate m = ( Functor m )  -- `()` is parsed as a type
#else
type EnterBoilerplate m =
  ( Enter (ServerT UserAPI m) m m (ServerT UserAPI m)
  , Enter (ServerT GroupAPI m) m m (ServerT GroupAPI m)
  )
#endif

type MoreEnterBoilerplate (m :: * -> *) api =
#if MIN_VERSION_servant_server(0,12,0)
  () ~ ()
#else
  Enter (ServerT api (SCIMHandler m)) (SCIMHandler m) Handler (Server api)
#endif

hoistServer :: forall api (m :: * -> *) (n :: * -> *).
               ( HasServer api '[]
#if !MIN_VERSION_servant_server(0,12,0)
               , Enter (ServerT api m) m Handler (ServerT api Handler)
               , Enter (ServerT api m) m n (ServerT api n)
#endif
               ) =>
               Proxy api -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n
#if MIN_VERSION_servant_server(0,12,0)
hoistServer = Servant.hoistServer
#else
hoistServer _ nt = enter (NT nt)
#endif


----------------------------------------------------------------------------
-- API implementation

siteServer ::
  forall m. (DB m, EnterBoilerplate (SCIMHandler m)) =>
  Configuration -> Site (AsServerT (SCIMHandler m))
siteServer conf = Site
  { config = toServant $ configServer conf
  , users = \auth ->
      hoistServer (Proxy @UserAPI) (addAuth auth) (toServant userServer)
  , groups = \auth ->
      hoistServer (Proxy @GroupAPI) (addAuth auth) (toServant groupServer)
  }
  where
    -- Perform authentication in a handler. This function will be applied to
    -- all leaves of the API.
    addAuth :: forall a. Maybe SCIMAuthData
            -> SCIMHandler m a
            -> SCIMHandler m a
    addAuth auth handler = do
      authResult <- authCheck auth
      case authResult of
        Authorized _ -> handler
        _ -> throwSCIM (unauthorized (scimAdmin <$> auth)
                                     (pack (show authResult)))

----------------------------------------------------------------------------
-- Server-starting utilities

type App m api =
  ( DB m
  , HasServer api '[]
  , EnterBoilerplate m
  , MoreEnterBoilerplate m api
  )

mkapp :: forall m api. (App m api)
      => Proxy api
      -> ServerT api (SCIMHandler m)
      -> (forall a. SCIMHandler m a -> Handler a)
      -> Application
mkapp proxy api nt =
  serve proxy $
    hoistServer proxy nt api

app :: forall m. App m SiteAPI
    => Configuration
    -> (forall a. SCIMHandler m a -> Handler a)
    -> Application
app c = mkapp (Proxy @SiteAPI) (toServant $ siteServer c)
