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
