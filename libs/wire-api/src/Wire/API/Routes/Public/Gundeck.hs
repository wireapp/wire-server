-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Gundeck where

import qualified Data.Swagger as Swagger
-- import Gundeck.Push

import Servant
import Servant.API.Generic (ToServantApi, (:-))
import Wire.API.Push.V2.Token
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Public

-- TODO: Figure this out from the existing handler
type AddTokenVerb = ()

data AddTokenResponse
  = AddTokenSuccess PushToken
  | AddTokenNoBudget
  | AddTokenNotFound
  | AddTokenInvalid
  | AddTokenTooLong
  | AddTokenMetadataTooLong

data Api routes = Api
  { addToken ::
      routes
        :- Summary "Register a native push token"
        :> ZLocalUser
        :> ZConn
        :> "push"
        :> "tokens"
        :> ReqBody '[JSON] PushToken
        :> MultiVerb 'POST '[JSON] AddTokenVerb AddTokenResponse
  }

type ServantAPI = ToServantApi Api

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
