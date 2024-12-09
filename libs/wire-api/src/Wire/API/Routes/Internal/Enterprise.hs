module Wire.API.Routes.Internal.Enterprise where

import Servant
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named

type InternalAPI = "i" :> InternalAPIBase

type InternalAPIBase =
  Named
    "status"
    ( "status" :> MultiVerb 'GET '[JSON] '[RespondEmpty 200 "OK"] ()
    )
