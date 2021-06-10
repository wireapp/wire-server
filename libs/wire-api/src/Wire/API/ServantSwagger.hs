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

module Wire.API.ServantSwagger where

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger (PathItem (..), Swagger (..))
import Imports

-- workaround for https://github.com/GetShopTV/swagger2/issues/218
-- We'd like to juse use (<>) but the instances are wrong
combinePathItem :: PathItem -> PathItem -> PathItem
combinePathItem s t =
  PathItem
    { _pathItemGet = _pathItemGet s <> _pathItemGet t,
      _pathItemPut = _pathItemPut s <> _pathItemPut t,
      _pathItemPost = _pathItemPost s <> _pathItemPost t,
      _pathItemDelete = _pathItemDelete s <> _pathItemDelete t,
      _pathItemOptions = _pathItemOptions s <> _pathItemOptions t,
      _pathItemHead = _pathItemHead s <> _pathItemHead t,
      _pathItemPatch = _pathItemPatch s <> _pathItemPatch t,
      _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
    }

combineSwagger :: Swagger -> Swagger -> Swagger
combineSwagger s t =
  Swagger
    { _swaggerInfo = _swaggerInfo s <> _swaggerInfo t,
      _swaggerHost = _swaggerHost s <|> _swaggerHost t,
      _swaggerBasePath = _swaggerBasePath s <|> _swaggerBasePath t,
      _swaggerSchemes = _swaggerSchemes s <> _swaggerSchemes t,
      _swaggerConsumes = _swaggerConsumes s <> _swaggerConsumes t,
      _swaggerProduces = _swaggerProduces s <> _swaggerProduces t,
      _swaggerPaths = InsOrdHashMap.unionWith combinePathItem (_swaggerPaths s) (_swaggerPaths t),
      _swaggerDefinitions = _swaggerDefinitions s <> _swaggerDefinitions t,
      _swaggerParameters = _swaggerParameters s <> _swaggerParameters t,
      _swaggerResponses = _swaggerResponses s <> _swaggerResponses t,
      _swaggerSecurityDefinitions = _swaggerSecurityDefinitions s <> _swaggerSecurityDefinitions t,
      _swaggerSecurity = _swaggerSecurity s <> _swaggerSecurity t,
      _swaggerTags = _swaggerTags s <> _swaggerTags t,
      _swaggerExternalDocs = _swaggerExternalDocs s <|> _swaggerExternalDocs t
    }
