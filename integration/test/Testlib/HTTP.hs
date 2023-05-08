module Testlib.HTTP where

import qualified Control.Exception as E
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Data.List
import Data.List.Split (splitOn)
import Data.String
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Testlib.Env
import Testlib.JSON
import Testlib.Types

splitHttpPath :: String -> [String]
splitHttpPath path = filter (not . null) (splitOn "/" path)

joinHttpPath :: [String] -> String
joinHttpPath = intercalate "/"

addJSONObject :: [Aeson.Pair] -> HTTP.Request -> HTTP.Request
addJSONObject = addJSON . Aeson.object

addJSON :: Aeson.Value -> HTTP.Request -> HTTP.Request
addJSON obj req =
  req
    { HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode obj),
      HTTP.requestHeaders =
        (fromString "Content-Type", fromString "application/json")
          : HTTP.requestHeaders req
    }

addHeader :: String -> String -> HTTP.Request -> HTTP.Request
addHeader name value req =
  req {HTTP.requestHeaders = (CI.mk . C8.pack $ name, C8.pack value) : HTTP.requestHeaders req}

addQueryParams :: [(String, String)] -> HTTP.Request -> HTTP.Request
addQueryParams params req =
  HTTP.setQueryString (map (\(k, v) -> (cs k, Just (cs v))) params) req

zUser :: String -> HTTP.Request -> HTTP.Request
zUser = addHeader "Z-User"

zConnection :: String -> HTTP.Request -> HTTP.Request
zConnection = addHeader "Z-Connection"

zType :: String -> HTTP.Request -> HTTP.Request
zType = addHeader "Z-Type"

contentTypeJSON :: HTTP.Request -> HTTP.Request
contentTypeJSON = addHeader "Content-Type" "application/json"

bindResponse :: HasCallStack => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k

bindResponseR :: HasCallStack => App Response -> (Response -> App a) -> App Response
bindResponseR m k = m >>= \r -> withResponse r k >> pure r

withResponse :: HasCallStack => Response -> (Response -> App a) -> App a
withResponse r k = onFailureAddResponse r (k r)

onFailureAddResponse :: Response -> App a -> App a
onFailureAddResponse r m = App $ do
  e <- ask
  liftIO $ E.catch (runAppWithEnv e m) $ \(AssertionFailure stack _ msg) -> do
    E.throw (AssertionFailure stack (Just r) msg)

data Versioned = Versioned | Unversioned | ExplicitVersion Int

baseRequest :: (HasCallStack, MakesValue domain) => domain -> Service -> Versioned -> String -> App HTTP.Request
baseRequest domain service versioned path = do
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
