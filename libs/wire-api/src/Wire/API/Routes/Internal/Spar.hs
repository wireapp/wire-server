module Wire.API.Routes.Internal.Spar where

import Control.Lens
import Data.Id
import Data.Swagger
import Imports
import Servant
import Servant.Swagger
import Wire.API.SwaggerServant
import Wire.API.User
import Wire.API.User.Saml

type InternalAPI =
  SwaggerTag "spar"
    :> "i"
    :> ( "status" :> Get '[JSON] NoContent
           :<|> "teams" :> Capture "team" TeamId :> DeleteNoContent
           :<|> "sso" :> "settings" :> ReqBody '[JSON] SsoSettings :> Put '[JSON] NoContent
           :<|> "scim" :> "userinfos" :> ReqBody '[JSON] UserSet :> Post '[JSON] ScimUserInfos
       )

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @InternalAPI)
    & info . title .~ "Wire-Server internal spar API"
