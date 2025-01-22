module Wire.API.Routes.Internal.Enterprise where

import Data.Domain
import Imports
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
               :> Capture "auth-token" Text
               :> Post '[JSON] DomainVerificationToken
           )
    :<|> Named
           "verify-domain-token"
           ( "verify-domain-token"
               :> Capture "domain" Domain
               :> Capture "auth-token" Text
               :> Post '[JSON] Bool
           )
