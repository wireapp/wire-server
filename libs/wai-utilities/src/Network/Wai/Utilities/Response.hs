{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wai.Utilities.Response where

import Data.Aeson hiding (Error, json)
import qualified Data.ByteString.Lazy as Lazy
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Utilities.Error

empty :: Response
empty = plain ""

noContent :: Response
noContent = empty & setStatus status204

plain :: Lazy.ByteString -> Response
plain = responseLBS status200 [plainContent]

plainContent :: Header
plainContent = (hContentType, "text/plain; charset=UTF-8")

json :: ToJSON a => a -> Response
json = responseLBS status200 [jsonContent] . encode

jsonContent :: Header
jsonContent = (hContentType, "application/json")

errorRs :: Status -> LText -> LText -> Response
errorRs s l m = errorRs' (Error s l m)

errorRs' :: Error -> Response
errorRs' e = setStatus (code e) (json e)

addHeader :: HeaderName -> ByteString -> Response -> Response
addHeader k v (ResponseFile s h f ff) = ResponseFile s ((k, v) : h) f ff
addHeader k v (ResponseBuilder s h b) = ResponseBuilder s ((k, v) : h) b
addHeader k v (ResponseStream s h x) = ResponseStream s ((k, v) : h) x
addHeader k v (ResponseRaw s r) = ResponseRaw s (addHeader k v r)

setStatus :: Status -> Response -> Response
setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
setStatus s (ResponseStream _ h x) = ResponseStream s h x
setStatus s (ResponseFile _ h f ff) = ResponseFile s h f ff
setStatus s (ResponseRaw x r) = ResponseRaw x (setStatus s r)
