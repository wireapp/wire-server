module Wire.API.Routes.Internal.Enterprise where

import Data.Domain
import Servant
import Wire.API.EnterpriseLogin
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named

type InternalAPI = "i" :> InternalAPIBase

type InternalAPIBase =
  Named
    "status"
    ( "status" :> MultiVerb 'GET '[JSON] '[RespondEmpty 200 "OK"] ()
    )
    :<|> Named
           "create-verification-token"
           ( "create-verification-token"
               :> Capture "domain" Domain
               :> Capture "auth-token" DomainVerificationAuthToken
               :> Post '[JSON] DomainVerificationToken
           )
