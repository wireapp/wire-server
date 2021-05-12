module Wire.API.Public.Combined where

import Data.Swagger

import qualified Wire.API.Public.Brig as Brig

swagger :: Swagger
swagger = Brig.swagger {- <> Galley.swagger -}
  & info . title .~ "Wire-Server API as Swagger 2.0 "
  & info . description ?~ "NOTE: only a few endpoints are visible here at the moment, more will come as we migrate them to Swagger 2.0. In the meantime please also look at the old swagger docs link for the not-yet-migrated endpoints. See https://docs.wire.com/understand/api-client-perspective/swagger.html for the old endpoints."
