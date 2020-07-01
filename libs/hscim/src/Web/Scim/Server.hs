{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Scim.Server
  ( -- * WAI application
    app,
    mkapp,
    App,

    -- * API tree
    SiteAPI,
    siteServer,

    -- ** API subtrees, useful for tests
    ConfigAPI,
    configServer,
    UserAPI,
    userServer,
    GroupAPI,
    groupServer,
  )
where

import GHC.Generics (Generic)
import Network.Wai
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Web.Scim.Capabilities.MetaSchema (ConfigSite, Configuration, configServer)
import Web.Scim.Class.Auth (AuthDB (..), AuthTypes (..))
import Web.Scim.Class.Group (GroupDB, GroupSite (..), GroupTypes (..), groupServer)
import Web.Scim.Class.User (UserDB (..), UserSite (..), userServer)
import Web.Scim.Handler

----------------------------------------------------------------------------
-- API specification

-- | A constraint indicating that monad @m@ supports operations with users and groups marked
-- with tag @t@.
type DB tag m = (UserDB tag m, GroupDB tag m, AuthDB tag m)

type ConfigAPI = ToServantApi ConfigSite

type UserAPI tag = ToServantApi (UserSite tag)

type GroupAPI tag = ToServantApi (GroupSite tag)

type SiteAPI tag = ToServantApi (Site tag)

data Site tag route = Site
  { config ::
      route
        :- ConfigAPI,
    users ::
      route
        :- Header "Authorization" (AuthData tag)
        :> "Users"
        :> UserAPI tag,
    groups ::
      route
        :- Header "Authorization" (AuthData tag)
        :> "Groups"
        :> GroupAPI tag
  }
  deriving (Generic)

----------------------------------------------------------------------------
-- API implementation

siteServer ::
  forall tag m.
  (DB tag m, Show (GroupId tag)) =>
  Configuration ->
  Site tag (AsServerT (ScimHandler m))
siteServer conf =
  Site
    { config = toServant $ configServer conf,
      users = \authData -> toServant (userServer @tag authData),
      groups = \authData -> toServant (groupServer @tag authData)
    }
  where

----------------------------------------------------------------------------
-- Server-starting utilities

type App tag m api =
  ( DB tag m,
    Show (GroupId tag),
    HasServer api '[]
  )

mkapp ::
  forall tag m api.
  (App tag m api) =>
  Proxy api ->
  ServerT api (ScimHandler m) ->
  (forall a. ScimHandler m a -> Handler a) ->
  Application
mkapp proxy api nt =
  serve proxy $
    hoistServer proxy nt api

app ::
  forall tag m.
  App tag m (SiteAPI tag) =>
  Configuration ->
  (forall a. ScimHandler m a -> Handler a) ->
  Application
app c =
  mkapp @tag
    (Proxy @(SiteAPI tag))
    (toServant $ siteServer c)
