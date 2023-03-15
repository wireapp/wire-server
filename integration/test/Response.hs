module Response where

import App
import Config
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Imports
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.URI (uriToString)

data Response = Response
  { json :: Maybe Aeson.Value,
    status :: Int,
    request :: HTTP.Request
  }

baseRequest :: Service -> String -> App HTTP.Request
baseRequest service path = do
  ctx <- getContext
  liftIO . HTTP.parseRequest $
    "http://localhost:" <> show (servicePort ctx.serviceMap service) <> path

addJSONObject :: [Aeson.Pair] -> HTTP.Request -> HTTP.Request
addJSONObject = addJSON . Aeson.object

addJSON :: Aeson.Value -> HTTP.Request -> HTTP.Request
addJSON obj req =
  req
    { HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode obj),
      HTTP.requestHeaders =
        ("Content-Type", "application/json")
          : HTTP.requestHeaders req
    }

submit :: ByteString -> HTTP.Request -> App Response
submit method req0 = do
  let req = req0 {HTTP.method = method}
  manager <- getManager
  res <- liftIO $ HTTP.httpLbs req manager
  pure $
    Response
      { json = Aeson.decode (HTTP.responseBody res),
        request = req,
        status = HTTP.statusCode (HTTP.responseStatus res)
      }

showRequest :: HTTP.Request -> String
showRequest r =
  T.unpack (T.decodeUtf8 (HTTP.method r))
    <> " "
    <> uriToString id (HTTP.getUri r) ""

getRequestBody :: HTTP.Request -> Maybe ByteString
getRequestBody req = case HTTP.requestBody req of
  HTTP.RequestBodyLBS lbs -> pure (L.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> Nothing

withResponse :: HasCallStack => Response -> (Response -> App a) -> App a
withResponse r k = onFailure (k r) $ do
  putStrLn $ replicate 80 '-'
  putStrLn $ "\x1b[38;5;11mrequest:\x1b[0m " <> showRequest r.request
  case getRequestBody r.request of
    Nothing -> pure ()
    Just b -> do
      putStrLn "\x1b[38;5;11mrequest body:\x1b[0m"
      T.putStrLn . T.decodeUtf8 $ case Aeson.decode (L.fromStrict b) of
        Just v -> L.toStrict (Aeson.encodePretty (v :: Aeson.Value))
        Nothing -> b
  putStrLn "\x1b[38;5;11mresponse body:\x1b[0m"
  T.putStrLn (T.decodeUtf8 (L.toStrict (foldMap Aeson.encodePretty r.json)))
  putStrLn $ replicate 80 '-'

bindResponse :: HasCallStack => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k
