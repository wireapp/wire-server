{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | swagger2 docs for galley generated with servant-swagger.  for now, this module contains
-- all of the servant code as well.
module Galley.API.Swagger
  ( GalleyRoutes,
    swagger,
  )
where

import Brig.Types.Team.LegalHold
import Data.Id
import Data.Proxy
import Data.Swagger hiding (Header (..))
import Servant.API hiding (Header)
import Servant.Swagger
import Wire.API.Team.Feature

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

-- FUTUREWORK: restructure this for readability and add missing bodies
type GalleyRoutesPublic =
  "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
    :> ReqBody '[JSON] NewLegalHoldService
    :> Post '[JSON] ViewLegalHoldService
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
      :> Get '[JSON] ViewLegalHoldService
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
      -- :> ReqBody '[JSON] RemoveLegalHoldSettingsRequest
      :> Verb 'DELETE 204 '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "consent"
      :> Post '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      :> Post '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId :> "approve"
      -- :> ReqBody '[JSON] ApproveLegalHoldForUserRequest
      :> Verb 'PUT 204 '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      :> Get '[JSON] UserLegalHoldStatusResponse
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      -- :> ReqBody '[JSON] DisableLegalHoldForUserRequest
      :> Verb 'DELETE 204 '[] NoContent

type GalleyRoutesInternal =
  "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
    :> Get '[JSON] (TeamFeatureStatus 'TeamFeatureLegalHold)
    :<|> "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
      :> ReqBody '[JSON] (TeamFeatureStatus 'TeamFeatureLegalHold)
      :> Put '[] NoContent

-- FUTUREWORK: move Swagger instances next to the types they describe

