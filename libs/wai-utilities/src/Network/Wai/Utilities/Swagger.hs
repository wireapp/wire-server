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

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Utilities.Swagger where

import Data.Swagger.Build.Api
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Imports
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Routing (Meta (..), Routes, attach, examine)
import Network.Wai.Utilities.Error
import Wire.Swagger

mkSwaggerApi :: Text -> [Model] -> Routes ApiBuilder m a -> ApiDecl
mkSwaggerApi base models sitemap =
  let routes = groupBy ((==) `on` routePath) (examine sitemap)
   in declare base "1.2" $ do
        resourcePath "/"
        produces "application/json"
        authorisation ApiKey
        mapM_ model models
        forM_ routes $ \r ->
          api (fixVars . decodeUtf8 $ routePath (head r)) $
            mapM_ routeMeta r
  where
    fixVars = Text.intercalate "/" . map var . Text.splitOn "/"
    var t
      | Text.null t = t
      | Text.head t == ':' = "{" <> Text.tail t <> "}"
      | otherwise = t

document :: Text -> Text -> OperationBuilder -> Routes ApiBuilder m ()
document x y = attach . operation x y

errorResponse :: Error -> OperationBuilder
errorResponse e = errorResponse' e errorModel

errorResponse' :: Error -> Model -> OperationBuilder
errorResponse' e md = response (statusCode (code e)) (renderError e) (model md)

renderError :: Error -> Text
renderError e = toStrict . toLazyText $ "[label=" <> lbl <> "] " <> msg
  where
    lbl = fromLazyText (label e)
    msg = fromLazyText (message e)
