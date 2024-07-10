module Wire.API.Routes.Internal.Cannon where

import Control.Lens ((.~))
import Data.OpenApi (HasInfo (info), HasTitle (title), OpenApi)
import Imports
import Servant
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named

type API =
  "i"
    :> Named
         "get-status"
         ( "status"
             :> MultiVerb
                  'GET
                  '[PlainText]
                  '[RespondEmpty 200 "Service is alive."]
                  ()
         )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @API)
    & info . title .~ "Wire-Server internal cannon API"
