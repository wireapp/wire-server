module Testlib.HTTP where

import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Data.Function
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Extra
import GHC.Generics
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hLocation)
import qualified Network.HTTP.Types as HTTP
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

addJSON :: (Aeson.ToJSON a) => a -> HTTP.Request -> HTTP.Request
addJSON obj = addBody (HTTP.RequestBodyLBS (Aeson.encode obj)) "application/json"

addXML :: ByteString -> HTTP.Request -> HTTP.Request
addXML xml = addBody (HTTP.RequestBodyBS xml) "application/xml"

addUrlEncodedForm :: [(String, String)] -> HTTP.Request -> HTTP.Request
addUrlEncodedForm form req =
  req
    { HTTP.requestBody = HTTP.RequestBodyLBS (L.fromStrict (HTTP.renderSimpleQuery False (both C8.pack <$> form))),
      HTTP.requestHeaders =
        (fromString "Content-Type", fromString "application/x-www-form-urlencoded")
          : HTTP.requestHeaders req
    }

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

contentTypeMixed :: HTTP.Request -> HTTP.Request
contentTypeMixed = addHeader "Content-Type" "multipart/mixed"

bindResponse :: (HasCallStack) => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k

infixl 1 `bindResponse`

withResponse :: (HasCallStack) => Response -> (Response -> App a) -> App a
withResponse r k = onFailureAddResponse r (k r)

-- | Check response status code, then return body.
getBody :: (HasCallStack) => Int -> Response -> App ByteString
getBody status = flip withResponse \resp -> do
  resp.status `shouldMatch` status
  pure resp.body

-- | Check response status code, then return JSON body.
getJSON :: (HasCallStack) => Int -> Response -> App Aeson.Value
getJSON status = flip withResponse \resp -> do
  resp.status `shouldMatch` status
  resp.json

-- | assert a response code in the 2** range
assertSuccess :: (HasCallStack) => Response -> App ()
assertSuccess = flip withResponse \resp -> resp.status `shouldMatchRange` (200, 299)

-- | assert a response status code
assertStatus :: (HasCallStack) => Int -> Response -> App ()
assertStatus status = flip withResponse \resp -> resp.status `shouldMatchInt` status

-- | assert a failure with some failure code and label
assertLabel :: (HasCallStack) => Int -> String -> Response -> App ()
assertLabel status label resp = do
  j <- getJSON status resp
  j %. "label" `shouldMatch` label

onFailureAddResponse :: (HasCallStack) => Response -> App a -> App a
onFailureAddResponse r m = App $ do
  e <- ask
  liftIO $ E.catch (runAppWithEnv e m) $ \(AssertionFailure stack _ msg) -> do
    E.throw (AssertionFailure stack (Just r) msg)

data Versioned = Versioned | Unversioned | ExplicitVersion Int
  deriving stock (Generic)

-- | If you don't know what domain is for or what you should put in there, try `rawBaseRequest
-- OwnDomain ...`.
rawBaseRequest :: (HasCallStack, MakesValue domain) => domain -> Service -> Versioned -> String -> App HTTP.Request
rawBaseRequest domain service versioned path = do
  domainV <- objDomain domain

  pathSegsPrefix <- case versioned of
    Versioned -> do
      d <- asString domainV
      versionMap <- asks (.apiVersionByDomain)
      v <- case Map.lookup d versionMap of
        Nothing -> asks (.defaultAPIVersion)
        Just v -> pure v
      pure ["v" <> show v]
    Unversioned -> pure []
    ExplicitVersion v -> do
      pure ["v" <> show v]

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

zProvider :: String -> HTTP.Request -> HTTP.Request
zProvider = addHeader "Z-Provider"

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
