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

-- | Local reproduction harness for the "federation RPC comes back as a 200 with
-- a non-JSON body" flake.
--
-- The federation @/rpc@ and @/federation@ endpoints are served over the same
-- keep-alive HTTP/1.1 port as @/i/metrics@. The observed failure is a @POST
-- /rpc/…@ returning a 200 whose body is an unrelated @/i/metrics@ page — i.e. a
-- response the connection should never have produced for that request. That only
-- happens if a response mis-frames the connection (leaving bytes behind) so a
-- later request on the reused connection reads them.
--
-- Two angles are exercised here:
--
--   1. 'testEmptyChunkDoesNotTruncate' drives the ACTUAL
--      'Federator.Response.streamingResponseToWai' with an upstream body that
--      yields an empty chunk in the middle of the stream (which a streamed HTTP/2
--      upstream legitimately can). If Warp turned that into a premature chunked
--      terminator, the response body would be truncated and the next pipelined
--      request would desync. This pins down whether the streaming path can poison
--      a keep-alive connection.
--
--   2. 'testConcurrentSharedManagerStaysInSync' stands up a federator-mimic (a
--      chunked @/i/metrics@ and an @/rpc@ served through 'streamingResponseToWai')
--      and hammers it through a SINGLE shared 'http-client' 'Manager' with a small
--      connection pool — the same setup the integration suite uses — mixing metric
--      scrapes and RPCs concurrently. It asserts no RPC response is ever poisoned
--      by connection reuse. It is a control: if well-formed responses never
--      desync here, the trigger must be a genuine mis-framing (see angle 1) rather
--      than the shared Manager alone.
module Test.Federator.Response (tests) where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (bracket)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as L
import Data.Sequence qualified as Seq
import Federator.Response (streamingResponseToWai)
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Numeric (readHex)
import Servant.Client.Core (ResponseF (..), StreamingResponse)
import Servant.Types.SourceT (source)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Response.streamingResponseToWai / keep-alive framing"
    [ testCase
        "empty upstream chunk does not truncate the streamed response or desync the connection"
        testEmptyChunkDoesNotTruncate,
      testCase
        "shared http-client Manager under concurrent metrics+RPC never delivers a poisoned RPC response"
        testConcurrentSharedManagerStaysInSync
    ]

-- | A genuine JSON federation response body (what @/brig/api-version@ returns).
realFederationBody :: ByteString
realFederationBody = "{\"supportedVersions\":[0,1,2]}"

-- | Build an upstream streaming response (as produced by the outward HTTP/2
-- call) carrying only a Content-Type header, so 'streamingResponseToWai' streams
-- it chunked, exactly like the real federation path.
jsonUpstream :: [ByteString] -> StreamingResponse
jsonUpstream chunks =
  Response
    { responseStatusCode = HTTP.ok200,
      responseHeaders = Seq.fromList [(HTTP.hContentType, "application/json")],
      responseHttpVersion = HTTP.http20,
      responseBody = source chunks
    }

--------------------------------------------------------------------------------
-- Angle 1: empty chunk in the middle of a streamed body.

-- | The upstream yields ["{\"a\":", "", "1}"] — an empty chunk between two
-- non-empty ones. A conforming server must still deliver the concatenation
-- ("{\"a\":1}") and keep the connection framed correctly.
emptyChunkUpstream :: StreamingResponse
emptyChunkUpstream = jsonUpstream ["{\"a\":", "", "1}"]

testEmptyChunkDoesNotTruncate :: Assertion
testEmptyChunkDoesNotTruncate =
  Warp.testWithApplication (pure (mkApp (streamingResponseToWai emptyChunkUpstream))) $ \port -> do
    raw <- singleRequestRaw port "/first"
    (r1, r2) <- pipelineTwo port
    putStrLn $
      unlines
        [ "",
          "===== empty-chunk framing =====",
          "raw bytes of GET /first: " <> show raw,
          "parsed 1st response: " <> show r1,
          "parsed 2nd response: " <> show r2
        ]
    assertEqual "first response body must be the full concatenation of all chunks" "{\"a\":1}" (hrBody r1)
    assertEqual "second (pipelined) response must be intact (connection not desynced)" legitSecond r2

--------------------------------------------------------------------------------
-- Angle 2: shared Manager under concurrency (control).

-- | A metrics page big enough to span several chunks, framed chunked (no
-- Content-Length) just like the prometheus middleware serves @/i/metrics@.
metricsBody :: ByteString
metricsBody =
  "# HELP http_request_duration_seconds The HTTP request latencies in seconds.\n"
    <> "# TYPE http_request_duration_seconds histogram\n"
    <> mconcat
      [ "http_request_duration_seconds_bucket{handler=\"/rpc\",le=\"" <> BS8.pack (show (i :: Int)) <> "\"} 1\n"
      | i <- [1 .. 400]
      ]

federatorMimicApp :: Wai.Application
federatorMimicApp req respond =
  case Wai.rawPathInfo req of
    "/i/metrics" ->
      respond $
        Wai.responseBuilder
          HTTP.ok200
          [(HTTP.hContentType, "text/plain; version=0.0.4")]
          (byteString metricsBody)
    _ -> respond (streamingResponseToWai (jsonUpstream [realFederationBody]))

testConcurrentSharedManagerStaysInSync :: Assertion
testConcurrentSharedManagerStaysInSync =
  Warp.testWithApplication (pure federatorMimicApp) $ \port -> do
    -- One shared Manager with a small pool, so RPC and metric requests are forced
    -- to reuse the same handful of keep-alive connections (as in the suite).
    mgr <- HTTP.newManager HTTP.defaultManagerSettings {HTTP.managerConnCount = 4}
    let n = 400 :: Int
        base = "http://127.0.0.1:" <> show port
        oneRequest i
          | even i = do
              req <- HTTP.parseRequest ("GET " <> base <> "/i/metrics")
              _ <- HTTP.httpLbs req mgr
              pure Nothing
          | otherwise = do
              req0 <- HTTP.parseRequest (base <> "/rpc/d.example.com/brig/api-version")
              resp <- HTTP.httpLbs req0 {HTTP.method = "POST"} mgr
              let body = L.toStrict (HTTP.responseBody resp)
                  ct = lookup HTTP.hContentType (HTTP.responseHeaders resp)
                  status = HTTP.statusCode (HTTP.responseStatus resp)
              pure $
                if status == 200 && ct == Just "application/json" && body == realFederationBody
                  then Nothing
                  else Just (i, status, ct, body)
    poisoned <- catMaybes <$> forConcurrently [1 .. n] oneRequest
    putStrLn $
      "\n===== shared-Manager concurrency ====="
        <> "\nRPC calls: "
        <> show (length (filter odd [1 .. n]))
        <> ", metric scrapes: "
        <> show (length (filter even [1 .. n]))
        <> ", poisoned RPC responses: "
        <> show (length poisoned)
    for_ poisoned $ \p -> putStrLn ("  POISONED: " <> show p)
    assertBool
      ("expected no RPC response to be poisoned by connection reuse, but got: " <> show poisoned)
      (null poisoned)

--------------------------------------------------------------------------------
-- Test server plumbing.

-- | Serve the given (already-rendered) response on @/first@; a distinct, plain
-- JSON response on anything else (the "next request" whose integrity we check).
mkApp :: Wai.Response -> Wai.Application
mkApp firstResp req respond =
  case Wai.rawPathInfo req of
    "/first" -> respond firstResp
    _ ->
      respond $
        Wai.responseLBS
          HTTP.ok200
          [(HTTP.hContentType, "application/json")]
          "{\"second\":true}"

legitSecond :: HttpResponse
legitSecond =
  HttpResponse
    { hrStatus = 200,
      hrContentType = Just "application/json",
      hrBody = "{\"second\":true}"
    }

--------------------------------------------------------------------------------
-- A minimal, conforming HTTP/1.1 client over a raw socket. It follows exactly
-- the framing the server advertises (Content-Length, else chunked), which is
-- where a mis-framed response bites a reused connection.

data HttpResponse = HttpResponse
  { hrStatus :: Int,
    hrContentType :: Maybe ByteString,
    hrBody :: ByteString
  }
  deriving (Eq, Show)

-- | Pipeline two requests on one keep-alive connection, then parse both
-- responses in order (deterministic byte interleaving).
pipelineTwo :: Int -> IO (HttpResponse, HttpResponse)
pipelineTwo port = do
  addr <- resolve port
  bracket (open addr) close $ \sock -> do
    buf <- newBuf sock
    sendAll sock $
      "GET /first HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
        <> "GET /second HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
    r1 <- parseResponse buf
    r2 <- parseResponse buf
    pure (r1, r2)

-- | Single request on a fresh connection; slurp bytes until the socket is idle
-- (keep-alive means no EOF). Evidence only.
singleRequestRaw :: Int -> ByteString -> IO ByteString
singleRequestRaw port path = do
  addr <- resolve port
  bracket (open addr) close $ \sock -> do
    sendAll sock $ "GET " <> path <> " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
    slurp sock mempty
  where
    slurp sock acc = do
      mchunk <- timeout 400_000 (recv sock 4096)
      case mchunk of
        Just chunk | not (BS.null chunk) -> slurp sock (acc <> chunk)
        _ -> pure acc

data Buf = Buf Socket (IORef ByteString)

newBuf :: Socket -> IO Buf
newBuf s = Buf s <$> newIORef mempty

fill :: Buf -> IO Bool
fill (Buf s ref) = do
  more <- recv s 4096
  if BS.null more then pure False else True <$ modifyIORef' ref (<> more)

readLineB :: Buf -> IO ByteString
readLineB buf@(Buf _ ref) = go
  where
    go = do
      b <- readIORef ref
      case breakOnCRLF b of
        Just (line, rest) -> line <$ writeIORef ref rest
        Nothing -> do
          ok <- fill buf
          if ok then go else b <$ writeIORef ref mempty

readNB :: Buf -> Int -> IO ByteString
readNB buf@(Buf _ ref) n = go
  where
    go = do
      b <- readIORef ref
      if BS.length b >= n
        then let (h, t) = BS.splitAt n b in h <$ writeIORef ref t
        else do
          ok <- fill buf
          if ok then go else b <$ writeIORef ref mempty

breakOnCRLF :: ByteString -> Maybe (ByteString, ByteString)
breakOnCRLF b =
  case BS.breakSubstring "\r\n" b of
    (pre, rest)
      | BS.null rest -> Nothing
      | otherwise -> Just (pre, BS.drop 2 rest)

parseResponse :: Buf -> IO HttpResponse
parseResponse buf = do
  statusLine <- readLineB buf
  hdrs <- readHeaders buf []
  body <- readBody buf hdrs
  pure
    HttpResponse
      { hrStatus = parseStatus statusLine,
        hrContentType = lookupCI "content-type" hdrs,
        hrBody = body
      }
  where
    parseStatus l
      | "HTTP/" `BS.isPrefixOf` l =
          case BS8.words l of
            (_ver : codeBS : _) -> fromMaybe (-1) (readMaybe (BS8.unpack codeBS))
            _ -> -1
      | otherwise = -1

readHeaders :: Buf -> [(ByteString, ByteString)] -> IO [(ByteString, ByteString)]
readHeaders buf acc = do
  line <- readLineB buf
  if BS.null line
    then pure (reverse acc)
    else readHeaders buf (splitHeader line : acc)
  where
    splitHeader line =
      case BS.breakSubstring ": " line of
        (k, rest)
          | BS.null rest -> (line, "")
          | otherwise -> (k, BS.drop 2 rest)

readBody :: Buf -> [(ByteString, ByteString)] -> IO ByteString
readBody buf hdrs =
  case lookupCI "content-length" hdrs of
    Just clBS | Just n <- readMaybe (BS8.unpack clBS) -> readNB buf n
    _ -> case lookupCI "transfer-encoding" hdrs of
      Just te | "chunked" `BS.isInfixOf` BS8.map toLower te -> readChunked buf mempty
      _ -> pure mempty

readChunked :: Buf -> ByteString -> IO ByteString
readChunked buf acc = do
  sizeLine <- readLineB buf
  let sizeHex = BS8.takeWhile (/= ';') sizeLine
  case readHex (BS8.unpack sizeHex) of
    [(0 :: Int, _)] -> acc <$ readLineB buf -- consume trailing CRLF after last chunk
    [(n, _)] -> do
      chunk <- readNB buf n
      _ <- readNB buf 2 -- trailing CRLF after the chunk
      readChunked buf (acc <> chunk)
    _ -> pure acc

lookupCI :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
lookupCI key = lookup key . map (first (BS8.map toLower))

resolve :: Int -> IO AddrInfo
resolve port =
  head
    <$> getAddrInfo
      (Just defaultHints {addrSocketType = Stream})
      (Just "127.0.0.1")
      (Just (show port))

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  pure sock
