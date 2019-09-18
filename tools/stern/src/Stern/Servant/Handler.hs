module Stern.Servant.Handler (rootPrefix, middleware, app, swaggerDoc) where

import Imports hiding (head)

import Data.Aeson (Value)
import Data.Id
import Data.Proxy
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import Network.Wai
import Servant.API.ContentTypes
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI
import Stern.Servant.Orphans ()
import Stern.Servant.Types

import qualified Data.Metrics.Middleware as Metrics
import qualified Data.Text as Text


rootPrefix :: [Text]
rootPrefix = ["servant"]

middleware :: [Text] -> Middleware
middleware prfx innerapp req cont = case stripPrefix prfx (pathInfo req) of
  Just stripped -> app (req' stripped) cont
  Nothing   -> innerapp req cont
  where
    req' pth = req
      { pathInfo = pth
      , rawPathInfo = cs $ "/" <> Text.intercalate "/" pth
      }

app :: Application
app = genericServe server

server :: API AsServer
server = API
  { _apiSwaggerDoc         = swaggerSchemaUIServer swaggerDoc
  , _apiInternalGetStatus  = apiInternalGetStatus
  , _apiInternalHeadStatus = apiInternalHeadStatus
  , _apiInternalMonitoring = apiInternalMonitoring
  , _apiSuspendUser        = apiSuspendUser
  , _apiUnsuspendUser      = apiUnsuspendUser
  }


swaggerDoc :: Swagger
swaggerDoc = toSwagger (genericApi (Proxy :: Proxy API))


apiInternalGetStatus :: Handler NoContent
apiInternalGetStatus = pure NoContent

apiInternalHeadStatus :: Handler NoContent
apiInternalHeadStatus = pure NoContent

apiInternalMonitoring :: Handler Value
apiInternalMonitoring = undefined{- TODO: @view metrics@ -} >>= Metrics.render


apiSuspendUser :: UserId -> Handler NoContent
apiSuspendUser _ = pure NoContent

apiUnsuspendUser :: UserId -> Handler NoContent
apiUnsuspendUser _ = pure NoContent
