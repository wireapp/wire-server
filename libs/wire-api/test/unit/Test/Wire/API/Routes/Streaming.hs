-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

-- | Regression tests for the two generic streaming response renderers in
-- @wire-api@: the 'MultiVerb' @SourceIO ByteString@ instance and the
-- 'LowLevelStream' server.
--
-- Both re-frame a streamed body via Warp (chunked). If they forward an upstream
-- (or type-level) @Content-Length@/@Transfer-Encoding@ header, Warp honours it
-- verbatim, and any mismatch between the declared length and the streamed bytes
-- desynchronises the caller's keep-alive HTTP/1.1 connection — the exact bug
-- fixed in @Federator.Response.streamingResponseToWai@. These tests assert that
-- neither renderer emits those framing headers, so the bug class cannot recur
-- here.
--
-- We inspect the emitted 'Network.Wai.Response' via 'Network.Wai.Test' (which
-- captures exactly the header list the response carries); the desync behaviour
-- of Warp itself is exercised end-to-end in the federator test suite.
module Test.Wire.API.Routes.Streaming (tests) where

import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as Seq
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Header (hTransferEncoding)
import Network.HTTP.Types.Method (StdMethod (GET))
import Network.Wai (Application, requestHeaders)
import Network.Wai.Test
import Servant.API (OctetStream, SourceIO, type (:>))
import Servant.Client.Core (ResponseF (..))
import Servant.Server (Server, serve)
import Servant.Types.SourceT (source)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.LowLevelStream (LowLevelStream, LowLevelStreamingBody)
import Wire.API.Routes.MultiVerb (responseToWai)

tests :: TestTree
tests =
  testGroup
    "Streaming response renderers drop framing headers"
    [ testCase
        "MultiVerb SourceIO renderer strips forwarded Content-Length/Transfer-Encoding"
        multiVerbStreamingStripsFramingHeaders,
      testCase
        "LowLevelStream server strips type-level Content-Length/Transfer-Encoding"
        lowLevelStreamStripsFramingHeaders
    ]

-- | Assert the common invariant on a served streaming response: framing headers
-- gone, unrelated headers kept, body intact.
assertNoFramingHeaders :: SResponse -> ByteString -> IO ()
assertNoFramingHeaders sresp expectedBody = do
  let hdrs = simpleHeaders sresp
  assertEqual "status" HTTP.status200 (simpleStatus sresp)
  assertBool
    ("Content-Length must be stripped, but headers were: " <> show hdrs)
    (isNothing (lookup HTTP.hContentLength hdrs))
  assertBool
    ("Transfer-Encoding must be stripped, but headers were: " <> show hdrs)
    (isNothing (lookup hTransferEncoding hdrs))
  assertEqual "Content-Type is preserved" (Just "application/octet-stream") (lookup HTTP.hContentType hdrs)
  assertEqual "unrelated header is preserved" (Just "yes") (lookup "X-Keep" hdrs)
  assertEqual "body is streamed intact" expectedBody (LBS.toStrict (simpleBody sresp))

--------------------------------------------------------------------------------
-- MultiVerb: the generic 'IsWaiBody (SourceIO ByteString)' renderer.

-- | An upstream streaming response carrying framing headers (as the outward
-- HTTP/2 federation response does). This is what would flow into the MultiVerb
-- renderer if a streaming endpoint ever surfaced an upstream length.
multiVerbUpstream :: ResponseF (SourceIO ByteString)
multiVerbUpstream =
  Response
    { responseStatusCode = HTTP.status200,
      responseHeaders =
        Seq.fromList
          [ (HTTP.hContentType, "application/octet-stream"),
            (HTTP.hContentLength, "999"),
            (hTransferEncoding, "chunked"),
            ("X-Keep", "yes")
          ],
      responseHttpVersion = HTTP.http11,
      responseBody = source ["ab", "cd"]
    }

multiVerbStreamingStripsFramingHeaders :: Assertion
multiVerbStreamingStripsFramingHeaders = do
  let app :: Application
      app _req respond = respond (responseToWai multiVerbUpstream)
  sresp <- runSession (request defaultRequest) app
  assertNoFramingHeaders sresp "abcd"

--------------------------------------------------------------------------------
-- LowLevelStream: the servant streaming server.

type StreamAPI =
  "s"
    :> LowLevelStream
         'GET
         200
         '[ '("Content-Length", "999"),
            '("Transfer-Encoding", "chunked"),
            '("X-Keep", "yes")
          ]
         "test stream"
         OctetStream

streamServer :: Server StreamAPI
streamServer = pure streamingBody
  where
    streamingBody :: LowLevelStreamingBody
    streamingBody = pure (\write flush -> write (byteString "hello") *> flush)

lowLevelStreamStripsFramingHeaders :: Assertion
lowLevelStreamStripsFramingHeaders = do
  let app = serve (Proxy @StreamAPI) streamServer
      req = (setPath defaultRequest "/s") {requestHeaders = [(HTTP.hAccept, "*/*")]}
  sresp <- runSession (request req) app
  assertNoFramingHeaders sresp "hello"
