{-# LANGUAGE OverloadedStrings #-}

module Bilge.Request
    ( RequestId (..)

      -- * Builders
    , empty
    , accept
    , body
    , bytes
    , lbytes
    , json
    , content
    , contentJson
    , contentProtobuf
    , header
    , host
    , path
    , paths
    , port
    , query
    , queryItem
    , queryItem'
    , secure
    , method
    , showRequest
    , noRedirect
    , timeout
    , expect2xx, expect3xx, expect4xx
    , checkStatus
    , cookie
    , cookieRaw
    , requestId
    , requestIdName

    -- * Re-exports
    , Request
    , Cookie      (..)
    , CookieJar
    , RequestBody (..)
    , Rq.parseUrl
    , Rq.applyBasicAuth
    , Rq.urlEncodedBody
    , Rq.getUri
    ) where

import Control.Exception
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString, intercalate)
import Data.CaseInsensitive (original)
import Data.Id (RequestId (..))
import Data.List (foldl')
import Data.Monoid
import Data.Word
import Network.HTTP.Client (Request, RequestBody (..), Cookie)
import Network.HTTP.Client.Internal (CookieJar (..), brReadSome, throwHttp)
import Network.HTTP.Types

import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as Lazy
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Network.HTTP.Client        as Rq
import qualified Network.HTTP.Types         as HTTP


-- Builders

-- | The empty request.
empty :: Request
empty = Rq.defaultRequest

host :: ByteString -> Request -> Request
host h r = r { Rq.host = h }

port :: Word16 -> Request -> Request
port p r = r { Rq.port = fromIntegral p }

method :: StdMethod -> Request -> Request
method m r = r { Rq.method = C.pack (show m) }

path :: ByteString -> Request -> Request
path p r = r { Rq.path = p }

paths :: [ByteString] -> Request -> Request
paths = path . intercalate "/"

-- | The request should be made over HTTPS.
secure :: Request -> Request
secure r = r { Rq.secure = True }

-- | Add a header field.
header :: HeaderName -> ByteString -> Request -> Request
header k v r = r { Rq.requestHeaders = (k, v) : Rq.requestHeaders r }

-- | Set complete query string (replacing previous content).
query :: Query -> Request -> Request
query q r = r { Rq.queryString = HTTP.renderQuery True q }

-- | Add query item to request.
queryItem' :: ByteString -> Maybe ByteString -> Request -> Request
queryItem' k v r
    | C.null (Rq.queryString r) = r { Rq.queryString = qstr True }
    | otherwise                 = r { Rq.queryString = Rq.queryString r <> "&" <> qstr False }
  where
    qstr b = HTTP.renderQuery b [(k, v)]

queryItem :: ByteString -> ByteString -> Request -> Request
queryItem k v = queryItem' k (Just v)

body :: RequestBody -> Request -> Request
body b r = r { Rq.requestBody = b }

-- | How many milliseconds to wait for response.
timeout :: Int -> Request -> Request
timeout t r = r { Rq.responseTimeout = Rq.responseTimeoutMicro (t * 1000) }

noRedirect :: Request -> Request
noRedirect r = r { Rq.redirectCount = 0 }

expect2xx :: Request -> Request
expect2xx = expectStatus ((== 2) . (`div` 100))

expect3xx :: Request -> Request
expect3xx = expectStatus ((== 3) . (`div` 100))

expect4xx :: Request -> Request
expect4xx = expectStatus ((== 4) . (`div` 100))

expectStatus :: (Int -> Bool) -> Request -> Request
expectStatus property r = r { Rq.checkResponse = check }
  where
    check _ res
      | property (HTTP.statusCode (Rq.responseStatus res)) = return ()
      | otherwise = do
          some <- Lazy.toStrict <$> brReadSome (Rq.responseBody res) 1024
          throwHttp $ Rq.StatusCodeException (const () <$> res) some

checkStatus :: (Status -> ResponseHeaders -> CookieJar -> Maybe SomeException) -> Request -> Request
checkStatus f r = r { Rq.checkResponse = check }
  where
    check _ res = case mayThrow res of
        Nothing -> return ()
        Just ex -> throwIO ex

    mayThrow res = f (Rq.responseStatus    res)
                     (Rq.responseHeaders   res)
                     (Rq.responseCookieJar res)

cookieRaw :: ByteString -> ByteString -> Request -> Request
cookieRaw k v = header "Cookie" (k <> "=" <> v)

cookie :: Cookie -> Request -> Request
cookie c r =
    case Rq.cookieJar r of
        Nothing      -> r { Rq.cookieJar = Just (CJ [c])    }
        Just (CJ cc) -> r { Rq.cookieJar = Just (CJ (c:cc)) }

requestId :: RequestId -> Request -> Request
requestId (RequestId rId) = header requestIdName rId

-- Convenience:

requestIdName :: HeaderName
requestIdName = "Request-Id"

bytes :: ByteString -> Request -> Request
bytes = body . RequestBodyBS

lbytes :: Lazy.ByteString -> Request -> Request
lbytes = body . RequestBodyLBS

json :: ToJSON a => a -> Request -> Request
json a = contentJson . lbytes (encode a)

accept :: ByteString -> Request -> Request
accept = header hAccept

content :: ByteString -> Request -> Request
content = header hContentType

contentJson :: Request -> Request
contentJson = content "application/json"

contentProtobuf :: Request -> Request
contentProtobuf = content "application/x-protobuf"

showRequest :: Request -> String
showRequest r =
      showString (C.unpack . Rq.method $ r)
    . showString " "
    . showString (C.unpack . Rq.path $ r)
    . showString (if Rq.secure r then " HTTPS/1.1\n" else " HTTP/1.1\n")
    . showHeaders
    . showString "\n\n"
    . showBody
    $ ""
  where
    showHeaders    = foldl' (.) (showString "") (map showHdr (Rq.requestHeaders r))
    showHdr (k, v) = showString . C.unpack $ original k <> ": " <> v <> "\n"

    showBody = case Rq.requestBody r of
        RequestBodyLBS lbs         -> showString (LC.unpack lbs)
        RequestBodyBS bs           -> showString (C.unpack bs)
        RequestBodyBuilder l _     -> showString ("RequestBodyBuilder<" ++ show l ++ ">")
        RequestBodyStream l _      -> showString ("RequestBodyStream<" ++ show l ++ ">")
        RequestBodyStreamChunked _ -> showString "RequestBodyStreamChunked"
        RequestBodyIO _            -> showString "RequestBodyIO"
