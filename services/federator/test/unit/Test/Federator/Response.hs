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

-- | Reproduction + regression harness for the "federation RPC comes back as a
-- 200 with a non-JSON body" flake.
--
-- The federator serves @/rpc@, @/federation@ and @/i/metrics@ over the same
-- keep-alive HTTP/1.1 port, and the integration suite hits them through a single
-- shared 'http-client' 'Manager' (connection pooling on). The observed failure
-- is a @POST /rpc/…@ returning a 200 whose body is an unrelated @/i/metrics@
-- page. That can only happen if a response mis-frames the connection, so the
-- client reads past the response boundary into the next response on a reused
-- connection.
--
-- 'testForwardedContentLengthDesync' pins the actual cause: the outward HTTP/2
-- response carries a @Content-Length@ header, and
-- 'Federator.Response.streamingResponseToWai' forwards it verbatim into a
-- 'Wai.responseStream'. Warp then HONOURS that length instead of chunking the
-- body it actually streams. If the declared length does not match the streamed
-- bytes (a truncated cold-start upstream, a reset mid-stream, a stale
-- @Content-Length@), the client reads the declared number of bytes and runs
-- straight into the following response. This test drives the REAL
-- 'streamingResponseToWai' and a byte-exact HTTP/1.1 client (which frames
-- exactly like 'http-client'): before the fix the RPC response is poisoned with
-- the next response's bytes; after the fix (strip framing headers, let Warp
-- frame the streamed body) the connection stays in sync.
--
-- 'testEmptyChunkDoesNotTruncate' and 'testConcurrentSharedManagerStaysInSync'
-- are controls: well-framed streamed responses never desync, so the shared
-- Manager / connection pool is not itself the bug.
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
        "forwarding an upstream Content-Length must not desync the reused connection"
        testForwardedContentLengthDesync,
      testCase
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

-- | Like 'jsonUpstream', but also carrying a @Content-Length@ header — exactly
-- what the real outward HTTP/2 response carries. @declaredLen@ is what the
-- header claims; @chunks@ is what actually gets streamed. In production these
-- can diverge (truncated/cold-start upstream); here we set them apart on purpose
-- to model that.
jsonUpstreamWithContentLength :: Int -> [ByteString] -> StreamingResponse
jsonUpstreamWithContentLength declaredLen chunks =
  Response
    { responseStatusCode = HTTP.ok200,
      responseHeaders =
        Seq.fromList
          [ (HTTP.hContentType, "application/json"),
            (HTTP.hContentLength, BS8.pack (show declaredLen))
          ],
      responseHttpVersion = HTTP.http20,
      responseBody = source chunks
    }

--------------------------------------------------------------------------------
-- The reproduction: a forwarded, mismatching Content-Length desyncs the
-- connection so the RPC request receives the *next* response (a metrics page).

-- | A recognisable @/i/metrics@-style page, served the way the prometheus
-- middleware serves it (its own framing, on the same keep-alive port).
metricsPage :: ByteString
metricsPage =
  "# HELP net_errors Number of exceptions caught by catchErrors middleware\n"
    <> "# TYPE net_errors counter\n"
    <> "net_errors 276.0\n"

-- | @/first@ is the federation RPC (served via the real 'streamingResponseToWai'
-- from an upstream that over-declares its Content-Length); @/second@ is a metrics
-- page. These are the two things multiplexed over one keep-alive connection.
desyncApp :: Wai.Application
desyncApp req respond =
  case Wai.rawPathInfo req of
    "/first" ->
      -- Upstream claims 100 bytes but only streams the (29-byte) body: a
      -- truncated cold-start response. This is the RPC the probe issues.
      respond (streamingResponseToWai (jsonUpstreamWithContentLength 100 [realFederationBody]))
    _ ->
      respond $
        Wai.responseBuilder
          HTTP.ok200
          [(HTTP.hContentType, "text/plain; version=0.0.4")]
          (byteString metricsPage)

testForwardedContentLengthDesync :: Assertion
testForwardedContentLengthDesync =
  Warp.testWithApplication (pure desyncApp) $ \port -> do
    (r1, r2) <- pipelineTwo port
    putStrLn $
      unlines
        [ "",
          "===== forwarded Content-Length desync =====",
          "RPC (/first) response: " <> show r1,
          "metrics (/second) response: " <> show r2
        ]
    -- The RPC response must be exactly the JSON body the upstream streamed —
    -- never contaminated with bytes from the following (metrics) response.
    assertEqual "RPC response status" 200 (hrStatus r1)
    assertEqual "RPC response content-type" (Just "application/json") (hrContentType r1)
    assertBool
      ( "RPC response body was poisoned by the next response on the reused connection: "
          <> show (hrBody r1)
      )
      (not ("# HELP" `BS.isInfixOf` hrBody r1) && not ("HTTP/1.1" `BS.isInfixOf` hrBody r1))
    assertEqual
      "RPC response body must be exactly the streamed body (no over-read)"
      realFederationBody
      (hrBody r1)
    -- ...and the following request's response must still be intact.
    assertEqual "pipelined metrics response status" 200 (hrStatus r2)
    assertEqual "pipelined metrics response content-type" (Just "text/plain; version=0.0.4") (hrContentType r2)
    assertEqual "pipelined metrics response body" metricsPage (hrBody r2)

--------------------------------------------------------------------------------
-- Control 1: empty chunk in the middle of a streamed body.

emptyChunkUpstream :: StreamingResponse
emptyChunkUpstream = jsonUpstream ["{\"a\":", "", "1}"]

testEmptyChunkDoesNotTruncate :: Assertion
testEmptyChunkDoesNotTruncate =
  Warp.testWithApplication (pure (mkApp (streamingResponseToWai emptyChunkUpstream))) $ \port -> do
    (r1, r2) <- pipelineTwo port
    assertEqual "first response body must be the full concatenation of all chunks" "{\"a\":1}" (hrBody r1)
    assertEqual "second (pipelined) response must be intact (connection not desynced)" legitSecond r2

--------------------------------------------------------------------------------
-- Control 2: shared Manager under concurrency (well-framed responses).

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
    for_ poisoned $ \p -> putStrLn ("  POISONED: " <> show p)
    assertBool
      ("expected no RPC response to be poisoned by connection reuse, but got: " <> show poisoned)
      (null poisoned)

--------------------------------------------------------------------------------
-- Test server plumbing.

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
-- the framing the server advertises (Content-Length, else chunked) — the same
-- choice 'http-client' makes — which is where a mis-framed response bites a
-- reused connection.

data HttpResponse = HttpResponse
  { hrStatus :: Int,
    hrContentType :: Maybe ByteString,
    hrBody :: ByteString
  }
  deriving (Eq, Show)

-- | Pipeline two requests on one keep-alive connection, then parse both
-- responses in order (deterministic byte interleaving — no timing races).
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

data Buf = Buf Socket (IORef ByteString)

newBuf :: Socket -> IO Buf
newBuf s = Buf s <$> newIORef mempty

fill :: Buf -> IO Bool
fill (Buf s ref) = do
  mmore <- timeout 1_000_000 (recv s 4096)
  case mmore of
    Just more | not (BS.null more) -> True <$ modifyIORef' ref (<> more)
    _ -> pure False

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
    [(0 :: Int, _)] -> acc <$ readLineB buf
    [(n, _)] -> do
      chunk <- readNB buf n
      _ <- readNB buf 2
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
