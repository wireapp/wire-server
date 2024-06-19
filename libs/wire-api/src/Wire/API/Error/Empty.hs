-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Error.Empty where

import Control.Lens ((.~))
import Data.OpenApi qualified as S
import Data.Text qualified as Text
import GHC.TypeLits
import Imports
import Network.HTTP.Types as HTTP
import Servant
import Servant.API.Status
import Servant.Client.Core
import Wire.API.Routes.MultiVerb

data EmptyErrorForLegacyReasons s desc

type instance ResponseType (EmptyErrorForLegacyReasons s desc) = ()

instance
  (KnownStatus s) =>
  IsResponse cs (EmptyErrorForLegacyReasons s desc)
  where
  type ResponseStatus (EmptyErrorForLegacyReasons s desc) = s
  type ResponseBody (EmptyErrorForLegacyReasons s desc) = ()

  responseRender _ () =
    pure $
      addContentType @PlainText
        Response
          { responseStatusCode = statusVal (Proxy @s),
            responseHeaders = mempty,
            responseBody = (),
            responseHttpVersion = HTTP.http11
          }

  responseUnrender _ output = guard (responseStatusCode output == statusVal (Proxy @s))

instance
  (KnownSymbol desc) =>
  IsSwaggerResponse (EmptyErrorForLegacyReasons s desc)
  where
  responseSwagger =
    pure $
      mempty
        & S.description
          .~ ( Text.pack (symbolVal (Proxy @desc))
                 <> "(**Note**: This error has an empty body for legacy reasons)"
             )
