module Response where

import App
import Config
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Records (HasField (..))
import Imports
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.URI (uriToString)

data Response = Response
  { jsonBody :: Maybe Aeson.Value,
    body :: ByteString,
    status :: Int,
    headers :: [HTTP.Header],
    request :: HTTP.Request
  }

instance HasField "json" Response (App Aeson.Value) where
  getField response = maybe (assertionFailure "Response has no json body") pure response.jsonBody

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

addHeader :: String -> String -> HTTP.Request -> HTTP.Request
addHeader name value req =
  req {HTTP.requestHeaders = (CI.mk . C8.pack $ name, C8.pack value) : HTTP.requestHeaders req}

zUser :: String -> HTTP.Request -> HTTP.Request
zUser = addHeader "Z-User"

zConnection :: String -> HTTP.Request -> HTTP.Request
zConnection = addHeader "Z-Connection"

submit :: ByteString -> HTTP.Request -> App Response
submit method req0 = do
  let req = req0 {HTTP.method = method}
  manager <- getManager
  res <- liftIO $ HTTP.httpLbs req manager
  pure $
    Response
      { jsonBody = Aeson.decode (HTTP.responseBody res),
        body = L.toStrict (HTTP.responseBody res),
        status = HTTP.statusCode (HTTP.responseStatus res),
        headers = HTTP.responseHeaders res,
        request = req
      }

showRequest :: HTTP.Request -> String
showRequest r =
  T.unpack (T.decodeUtf8 (HTTP.method r))
    <> " "
    <> uriToString id (HTTP.getUri r) ""

showHeaders :: [HTTP.Header] -> String
showHeaders r =
  intercalate "\n" $
    r <&> \(name, value) ->
      C8.unpack (CI.original name) <> ": " <> C8.unpack value

getRequestBody :: HTTP.Request -> Maybe ByteString
getRequestBody req = case HTTP.requestBody req of
  HTTP.RequestBodyLBS lbs -> pure (L.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> Nothing

printHLine :: IO ()
printHLine = do
  putStrLn $ replicate 20 '-'

printResponse :: Response -> IO ()
printResponse r = do
  printHLine
  putStrLn $ colored yellow "request: \n" <> showRequest r.request
  putStrLn $ colored yellow "request headers: \n" <> showHeaders (HTTP.requestHeaders r.request)
  case getRequestBody r.request of
    Nothing -> pure ()
    Just b -> do
      putStrLn (colored yellow "request body:")
      T.putStrLn . T.decodeUtf8 $ case Aeson.decode (L.fromStrict b) of
        Just v -> L.toStrict (Aeson.encodePretty (v :: Aeson.Value))
        Nothing -> b
  putStrLn (colored blue "response status: " <> show r.status)
  putStrLn $ colored blue "response headers: \n" <> showHeaders r.headers
  putStrLn (colored blue "response body:")
  T.putStrLn
    ( T.decodeUtf8 $
        case r.jsonBody of
          Just b -> L.toStrict (Aeson.encodePretty b)
          Nothing -> r.body
    )
  printHLine

withResponse :: HasCallStack => Response -> (Response -> App a) -> App a
withResponse r k = onFailure (k r) (printResponse r)

bindResponse :: HasCallStack => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k
