{-# LANGUAGE AllowAmbiguousTypes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Web.Scim.Server
  ( -- * WAI application
    app,
    mkapp,
    App,

    -- * API tree
    SiteAPI,
    Site (..),
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
  (DB tag m) =>
  Configuration ->
  Site tag (AsServerT (ScimHandler m))
siteServer conf =
  Site
    { config = toServant $ configServer conf,
      users = \authData -> toServant (userServer @tag authData),
      groups = \authData -> toServant (groupServer @tag authData)
    }

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
  (App tag m (SiteAPI tag)) =>
  Configuration ->
  (forall a. ScimHandler m a -> Handler a) ->
  Application
app c =
  mkapp @tag
    (Proxy @(SiteAPI tag))
    (toServant $ siteServer c)
