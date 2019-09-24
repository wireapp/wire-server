{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}
-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | swagger2 docs for galley generated with servant-swagger.  for now, this module contains
-- all of the servant code as well.
module Galley.API.Swagger (GalleyRoutes, swagger) where

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Brig.Types.Servant ()
import Brig.Types.Team.LegalHold
import Data.Id
import Data.Proxy
import Servant.API hiding (Header)
import Servant.Swagger

{-
import Data.String.Conversions
import System.Process (system)
import Data.Aeson (encode)
import Test.Hspec (hspec)
import Brig.Types.Test.Arbitrary ()

main :: IO ()
main = do
  writeFile "/tmp/x" . cs $ encode swagger
  void $ system "cat /tmp/x | json_pp && curl -X POST -d @/tmp/x -H 'Content-Type:application/json' http://online.swagger.io/validator/debug | json_pp"
  hspec $ validateEveryToJSON (Proxy @GalleyRoutes)
  -- see also: https://github.com/swagger-api/validator-badge

  -- alternatives:
  -- https://github.com/navidsh/maven.swagger.validator
  -- https://editor.swagger.io/  (this finds dangling refs.  good.)
  -- https://apidevtools.org/swagger-parser/online/  (also finds dangling refs, but it's *very slow*)
-}


swagger :: Swagger
swagger = toSwagger (Proxy @GalleyRoutes)


type GalleyRoutes = GalleyRoutesPublic :<|> GalleyRoutesInternal

type GalleyRoutesPublic
     = "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> ReqBody '[JSON] NewLegalHoldService
          :> Post '[JSON] ViewLegalHoldService
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> Get '[JSON] ViewLegalHoldService
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> Verb 'DELETE 204 '[] NoContent

  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
          :> Post '[] NoContent
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId :> "approve"
          :> Verb 'PUT 204 '[] NoContent
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
          :> Get '[JSON] UserLegalHoldStatusResponse
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
          :> Verb 'DELETE 204 '[] NoContent

type GalleyRoutesInternal
     = "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
          :> Get '[JSON] LegalHoldTeamConfig
  :<|> "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
          :> ReqBody '[JSON] LegalHoldTeamConfig
          :> Put '[] NoContent
