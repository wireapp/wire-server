{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Web.SCIM.Server
  ( app
  , SiteAPI
  , mkapp, App
  ) where

import           Web.SCIM.Class.User (UserSite (..), UserDB, userServer)
import           Web.SCIM.Class.Group (GroupSite (..), GroupDB, groupServer)
import           Web.SCIM.Capabilities.MetaSchema (ConfigAPI, Configuration, configServer)
import           Control.Monad.Except
import           GHC.Generics (Generic)
import           Network.Wai
import           Servant
import           Servant.Generic
#if !MIN_VERSION_servant_server(0,12,0)
import           Servant.Utils.Enter
#endif


type SCIMHandler m = (MonadError ServantErr m, UserDB m, GroupDB m)
type SiteAPI = ToServant (Site AsApi)

data Site route = Site
  { config :: route :- ToServant (ConfigAPI AsApi)
  , users :: route :- "Users" :> ToServant (UserSite AsApi)
  , groups :: route :- "Groups" :> ToServant (GroupSite AsApi)
  } deriving (Generic)

siteServer :: Configuration -> SCIMHandler m => Site (AsServerT m)
siteServer conf = Site
  { config = toServant $ configServer conf
  , users = toServant userServer
  , groups = toServant groupServer
  }

type App m api = ( SCIMHandler m
                 , HasServer api '[]
#if !MIN_VERSION_servant_server(0,12,0)
                 , Enter (ServerT api m) m Handler (Server api)
#endif
                 )

mkapp :: forall m api. (App m api)
      => Proxy api -> ServerT api m -> (forall a. m a -> Handler a) -> Application
mkapp proxy api nt = serve proxy $
#if MIN_VERSION_servant_server(0,12,0)
  hoistServer proxy nt
#else
  enter (NT nt)
#endif
  api

app :: forall m. App m SiteAPI => Configuration -> (forall a. m a -> Handler a) -> Application
app c = mkapp (Proxy :: Proxy SiteAPI) (toServant $ siteServer c)
