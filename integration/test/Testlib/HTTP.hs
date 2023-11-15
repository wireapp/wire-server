module Testlib.HTTP where

import Control.Exception qualified as E
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as L
import Data.CaseInsensitive qualified as CI
import Data.Function
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.String
import Data.String.Conversions (cs)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hLocation)
import Network.HTTP.Types qualified as HTTP
import Network.URI (URI (..), URIAuth (..), parseURI)
import Testlib.Assertions
import Testlib.Env
import Testlib.JSON
import Testlib.Types
import Prelude

splitHttpPath :: String -> [String]
splitHttpPath path = filter (not . null) (splitOn "/" path)

joinHttpPath :: [String] -> String
joinHttpPath = intercalate "/"

addJSONObject :: [Aeson.Pair] -> HTTP.Request -> HTTP.Request
addJSONObject = addJSON . Aeson.object

addJSON :: Aeson.ToJSON a => a -> HTTP.Request -> HTTP.Request
addJSON obj = addBody (HTTP.RequestBodyLBS (Aeson.encode obj)) "application/json"

addBody :: HTTP.RequestBody -> String -> HTTP.Request -> HTTP.Request
addBody body contentType req =
  req
    { HTTP.requestBody = body,
      HTTP.requestHeaders =
        (fromString "Content-Type", fromString contentType)
          : HTTP.requestHeaders req
    }

addMLS :: ByteString -> HTTP.Request -> HTTP.Request
addMLS bytes req =
  req
    { HTTP.requestBody = HTTP.RequestBodyBS bytes,
      HTTP.requestHeaders =
        (fromString "Content-Type", fromString "message/mls")
          : HTTP.requestHeaders req
    }

addProtobuf :: ByteString -> HTTP.Request -> HTTP.Request
addProtobuf bytes req =
  req
    { HTTP.requestBody = HTTP.RequestBodyBS bytes,
      HTTP.requestHeaders = (fromString "Content-Type", fromString "application/x-protobuf") : HTTP.requestHeaders req
    }

addHeader :: String -> String -> HTTP.Request -> HTTP.Request
addHeader name value req =
  req {HTTP.requestHeaders = (CI.mk . C8.pack $ name, C8.pack value) : HTTP.requestHeaders req}

setCookie :: String -> HTTP.Request -> HTTP.Request
setCookie c r =
  addHeader "Cookie" (cs c) r

addQueryParams :: [(String, String)] -> HTTP.Request -> HTTP.Request
addQueryParams params req =
  HTTP.setQueryString (map (\(k, v) -> (cs k, Just (cs v))) params) req

contentTypeJSON :: HTTP.Request -> HTTP.Request
contentTypeJSON = addHeader "Content-Type" "application/json"

bindResponse :: HasCallStack => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k

withResponse :: HasCallStack => Response -> (Response -> App a) -> App a
withResponse r k = onFailureAddResponse r (k r)

-- | Check response status code, then return body.
getBody :: HasCallStack => Int -> Response -> App ByteString
getBody status resp = withResponse resp $ \r -> do
  r.status `shouldMatch` status
  pure r.body

-- | Check response status code, then return JSON body.
getJSON :: HasCallStack => Int -> Response -> App Aeson.Value
getJSON status resp = withResponse resp $ \r -> do
  r.status `shouldMatch` status
  r.json

assertSuccess :: HasCallStack => Response -> App ()
assertSuccess resp = withResponse resp $ \r -> r.status `shouldMatchRange` (200, 299)

onFailureAddResponse :: HasCallStack => Response -> App a -> App a
onFailureAddResponse r m = App $ do
  e <- ask
  liftIO $ E.catch (runAppWithEnv e m) $ \(AssertionFailure stack _ msg) -> do
    E.throw (AssertionFailure stack (Just r) msg)

data Versioned = Versioned | Unversioned | ExplicitVersion Int

-- | If you don't know what domain is for or what you should put in there, try `rawBaseRequest
-- OwnDomain ...`.
rawBaseRequest :: (HasCallStack, MakesValue domain) => domain -> Service -> Versioned -> String -> App HTTP.Request
rawBaseRequest domain service versioned path = do
  pathSegsPrefix <- case versioned of
    Versioned -> do
      v <- asks (.defaultAPIVersion)
      pure ["v" <> show v]
    Unversioned -> pure []
    ExplicitVersion v -> do
      pure ["v" <> show v]

  domainV <- objDomain domain
  serviceMap <- getServiceMap domainV

  liftIO . HTTP.parseRequest $
    let HostPort h p = serviceHostPort serviceMap service
     in "http://" <> h <> ":" <> show p <> ("/" <> joinHttpPath (pathSegsPrefix <> splitHttpPath path))

baseRequest :: (HasCallStack, MakesValue user) => user -> Service -> Versioned -> String -> App HTTP.Request
baseRequest user service versioned path = do
  req <- rawBaseRequest user service versioned path
  uid <- objId user
  cli <-
    make user >>= \case
      Aeson.Object _ -> do
        c <- lookupField user "client_id"
        traverse asString c
      _ -> pure Nothing
  pure $ req & zUser uid & maybe id zClient cli & zConnection "conn"

zUser :: String -> HTTP.Request -> HTTP.Request
zUser = addHeader "Z-User"

zConnection :: String -> HTTP.Request -> HTTP.Request
zConnection = addHeader "Z-Connection"

zClient :: String -> HTTP.Request -> HTTP.Request
zClient = addHeader "Z-Client"

zType :: String -> HTTP.Request -> HTTP.Request
zType = addHeader "Z-Type"

zHost :: String -> HTTP.Request -> HTTP.Request
zHost = addHeader "Z-Host"

submit :: String -> HTTP.Request -> App Response
submit method req0 = do
  let req = req0 {HTTP.method = T.encodeUtf8 (T.pack method)}
  manager <- asks (.manager)
  res <- liftIO $ HTTP.httpLbs req manager
  pure $
    Response
      { jsonBody = Aeson.decode (HTTP.responseBody res),
        body = L.toStrict (HTTP.responseBody res),
        status = HTTP.statusCode (HTTP.responseStatus res),
        headers = HTTP.responseHeaders res,
        request = req
      }

locationHeaderHost :: Response -> String
locationHeaderHost resp =
  let location = C8.unpack . snd . fromJust $ locationHeader resp
      locationURI = fromJust $ parseURI location
      locationHost = uriRegName (fromJust (locationURI & uriAuthority))
   in locationHost

locationHeader :: Response -> Maybe (HTTP.HeaderName, ByteString)
locationHeader = findHeader hLocation

findHeader :: HTTP.HeaderName -> Response -> Maybe (HTTP.HeaderName, ByteString)
findHeader name resp = find (\(name', _) -> name == name') resp.headers
