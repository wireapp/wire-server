{-# LANGUAGE OverloadedStrings #-}

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

module Bilge.Request
  ( RequestId (..),

    -- * Builders
    empty,
    accept,
    acceptJson,
    acceptProtobuf,
    body,
    bytes,
    lbytes,
    lbytesChunkedIO,
    lbytesRefChunked,
    lbytesRefPopper,
    json,
    jsonChunkedIO,
    content,
    contentJson,
    contentProtobuf,
    header,
    host,
    path,
    paths,
    port,
    query,
    queryItem,
    queryItem',
    secure,
    method,
    showRequest,
    noRedirect,
    timeout,
    expect2xx,
    expect3xx,
    expect4xx,
    expectStatus,
    checkStatus,
    cookie,
    cookieRaw,
    requestId,
    requestIdName,
    extHost,
    extPort,

    -- * Re-exports
    Request,
    Cookie (..),
    CookieJar,
    RequestBody (..),
    Rq.parseRequest,
    Rq.applyBasicAuth,
    Rq.urlEncodedBody,
    Rq.getUri,
  )
where

import Control.Exception
import Control.Lens
import Data.Aeson (ToJSON, encode)
import Data.ByteString (intercalate)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.CaseInsensitive (original)
import Data.Id (RequestId (..))
import Imports hiding (intercalate)
import Network.HTTP.Client (Cookie, GivesPopper, Request, RequestBody (..))
import Network.HTTP.Client qualified as Rq
import Network.HTTP.Client.Internal (CookieJar (..), brReadSome, throwHttp)
import Network.HTTP.Types
import Network.HTTP.Types qualified as HTTP
import URI.ByteString qualified as URI

-- Builders

-- | The empty request.
empty :: Request
empty = Rq.defaultRequest

host :: ByteString -> Request -> Request
host h r = r {Rq.host = h}

port :: Word16 -> Request -> Request
port p r = r {Rq.port = fromIntegral p}

method :: StdMethod -> Request -> Request
method m r = r {Rq.method = C.pack (show m)}

path :: ByteString -> Request -> Request
path p r = r {Rq.path = p}

paths :: [ByteString] -> Request -> Request
paths = path . intercalate "/"

-- | The request should be made over HTTPS.
secure :: Request -> Request
secure r = r {Rq.secure = True}

-- | Add a header field.
header :: HeaderName -> ByteString -> Request -> Request
header k v r = r {Rq.requestHeaders = (k, v) : Rq.requestHeaders r}

-- | Set complete query string (replacing previous content).
query :: Query -> Request -> Request
query q r = r {Rq.queryString = HTTP.renderQuery True q}

-- | Add query item to request.
queryItem' :: ByteString -> Maybe ByteString -> Request -> Request
queryItem' k v r
  | C.null (Rq.queryString r) = r {Rq.queryString = qstr True}
  | otherwise = r {Rq.queryString = Rq.queryString r <> "&" <> qstr False}
  where
    qstr b = HTTP.renderQuery b [(k, v)]

queryItem :: ByteString -> ByteString -> Request -> Request
queryItem k v = queryItem' k (Just v)

body :: RequestBody -> Request -> Request
body b r = r {Rq.requestBody = b}

-- | How many milliseconds to wait for response.
timeout :: Int -> Request -> Request
timeout t r = r {Rq.responseTimeout = Rq.responseTimeoutMicro (t * 1000)}

noRedirect :: Request -> Request
noRedirect r = r {Rq.redirectCount = 0}

expect2xx :: Request -> Request
expect2xx = expectStatus ((== 2) . (`div` 100))

expect3xx :: Request -> Request
expect3xx = expectStatus ((== 3) . (`div` 100))

expect4xx :: Request -> Request
expect4xx = expectStatus ((== 4) . (`div` 100))

expectStatus :: (Int -> Bool) -> Request -> Request
expectStatus property r = r {Rq.checkResponse = check}
  where
    check _ res
      | property (HTTP.statusCode (Rq.responseStatus res)) = pure ()
      | otherwise = do
          some <- Lazy.toStrict <$> brReadSome (Rq.responseBody res) 1024
          throwHttp $ Rq.StatusCodeException (void res) some

checkStatus :: (Status -> ResponseHeaders -> CookieJar -> Maybe SomeException) -> Request -> Request
checkStatus f r = r {Rq.checkResponse = check}
  where
    check _ res = forM_ (mayThrow res) throwIO
    mayThrow res =
      f
        (Rq.responseStatus res)
        (Rq.responseHeaders res)
        (Rq.responseCookieJar res)

cookieRaw :: ByteString -> ByteString -> Request -> Request
cookieRaw k v = header "Cookie" (k <> "=" <> v)

cookie :: Cookie -> Request -> Request
cookie c r =
  case Rq.cookieJar r of
    Nothing -> r {Rq.cookieJar = Just (CJ [c])}
    Just (CJ cc) -> r {Rq.cookieJar = Just (CJ (c : cc))}

requestId :: RequestId -> Request -> Request
requestId (RequestId rId) = header requestIdName rId

-- Convenience:

requestIdName :: HeaderName
requestIdName = "Request-Id"

bytes :: ByteString -> Request -> Request
bytes = body . RequestBodyBS

lbytes :: Lazy.ByteString -> Request -> Request
lbytes = body . RequestBodyLBS

-- | Not suitable for @a@ which translates to very large JSON (more than a few megabytes) as the
-- bytestring produced by JSON will get computed and stored as it is in memory
-- in order to compute the @Content-Length@ header. For making a request with
-- big JSON objects, please use @lbytesRefChunked@
json :: (ToJSON a) => a -> Request -> Request
json a = contentJson . lbytes (encode a)

-- | Like @lbytesChunkedIO@ but for sending a JSON body
jsonChunkedIO :: (ToJSON a, MonadIO m) => a -> m (Request -> Request)
jsonChunkedIO a = do
  (contentJson .) <$> lbytesChunkedIO (encode a)

-- | Makes requests with @Transfer-Encoding: chunked@ and no @Content-Length@
-- header. Tries to ensures that the lazy bytestring is garbage collected as a
-- "chunk" of this bytestring is consumed. Note that it is not possible to
-- guarantee garbage collection as something else holding a reference to this
-- bytestring could stop that from happening.
--
-- A more straightforward function like this will keep the reference to the
-- complete bytestring, which might be against the idea of using chunked
-- encoding:
--
-- @
-- lbytesChunked bs = body (RequestBodyStreamChunked $ lbytesPopper bs)
-- lbytesPopper bs needsPopper = do
--   ref <- newIORef $ LC.toChunks bs
--   lbytesRefPopper ref needsPopper
-- @
--
-- This is because the closure for @lbytesPopper@ keeps the reference to @bs@
-- alive. To avoid this, this function allocates an @IORef@ and passes that to
-- @lbytesRefChunked@.
lbytesChunkedIO :: (MonadIO m) => Lazy.ByteString -> m (Request -> Request)
lbytesChunkedIO bs = do
  chunksRef <- newIORef $ Lazy.toChunks bs
  pure $ lbytesRefChunked chunksRef

-- | Takes @IORef@ to chunks of strict @ByteString@ (perhaps) from a lazy
-- @Lazy.ByteString@, this helps the lazy bytestring get garbage collected as it
-- gets consumed. The request made will have @Transfer-Encoding: chunked@ and no
-- @Content-Length@ header.
--
-- See @lbytesChunkedIO@ for reference usage.
lbytesRefChunked :: IORef [ByteString] -> Request -> Request
lbytesRefChunked chunksRef =
  body (RequestBodyStreamChunked $ lbytesRefPopper chunksRef)

lbytesRefPopper :: IORef [ByteString] -> GivesPopper ()
lbytesRefPopper chunksRef needsPopper = do
  let popper = do
        atomicModifyIORef chunksRef $ \case
          [] -> ([], mempty)
          (c : cs) -> (cs, c)
  needsPopper popper

accept :: ByteString -> Request -> Request
accept = header hAccept

acceptJson :: Request -> Request
acceptJson = accept "application/json"

acceptProtobuf :: Request -> Request
acceptProtobuf = accept "application/x-protobuf"

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
    showHeaders = foldl' (.) (showString "") (map showHdr (Rq.requestHeaders r))
    showHdr (k, v) = showString . C.unpack $ original k <> ": " <> v <> "\n"
    showBody = case Rq.requestBody r of
      RequestBodyLBS lbs -> showString (LC.unpack lbs)
      RequestBodyBS bs -> showString (C.unpack bs)
      RequestBodyBuilder l _ -> showString ("RequestBodyBuilder<" ++ show l ++ ">")
      RequestBodyStream l _ -> showString ("RequestBodyStream<" ++ show l ++ ">")
      RequestBodyStreamChunked _ -> showString "RequestBodyStreamChunked"
      RequestBodyIO _ -> showString "RequestBodyIO"

-- uri-bytestring

extHost :: URI.URI -> Maybe ByteString
extHost u = u ^. URI.authorityL <&> view (URI.authorityHostL . URI.hostBSL)

extPort :: URI.URI -> Maybe Word16
extPort u = do
  a <- u ^. URI.authorityL
  p <- a ^. URI.authorityPortL
  pure (fromIntegral (p ^. URI.portNumberL))
