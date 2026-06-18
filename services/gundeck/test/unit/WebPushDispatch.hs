-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Unit tests for the pure dispatch helpers in "Gundeck.Push.Web".
--
-- These cover the response classification, RFC 8030 header value mapping, and
-- endpoint origin extraction — the pure decision logic that 'push1' builds on.
-- The Gundeck-monadic dispatch path itself is covered by integration tests.
module WebPushDispatch
  ( tests,
  )
where

import Gundeck.Push.Web
import Imports
import Network.HTTP.Types.Status
  ( status200,
    status201,
    status202,
    status400,
    status401,
    status403,
    status404,
    status410,
    status413,
    status429,
    status500,
    status502,
    status503,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Push.V2 (Priority (HighPriority, LowPriority))
import Wire.API.Push.V2.WebSubscription (EndpointUrl, mkEndpointUrl)

tests :: TestTree
tests =
  testGroup
    "WebPushDispatch"
    [ testGroup
        "classifyResponse"
        [ testCase "201 Created → ResponseSuccess" $
            classifyResponse status201 @?= ResponseSuccess,
          testCase "202 Accepted → ResponseSuccess" $
            classifyResponse status202 @?= ResponseSuccess,
          testCase "404 Not Found → ResponseGone" $
            classifyResponse status404 @?= ResponseGone,
          testCase "410 Gone → ResponseGone" $
            classifyResponse status410 @?= ResponseGone,
          testCase "413 Payload Too Large → ResponseTooLarge" $
            classifyResponse status413 @?= ResponseTooLarge,
          testCase "429 Too Many Requests → ResponseRetryable" $
            classifyResponse status429 @?= ResponseRetryable,
          testCase "500 Internal Server Error → ResponseRetryable" $
            classifyResponse status500 @?= ResponseRetryable,
          testCase "502 Bad Gateway → ResponseRetryable" $
            classifyResponse status502 @?= ResponseRetryable,
          testCase "503 Service Unavailable → ResponseRetryable" $
            classifyResponse status503 @?= ResponseRetryable,
          testCase "200 OK (unexpected) → ResponseError" $
            -- 200 is not in RFC 8030's success set (201/202); treat as
            -- a permanent client error rather than silently accepting.
            classifyResponse status200 @?= ResponseError,
          testCase "400 Bad Request → ResponseError" $
            classifyResponse status400 @?= ResponseError,
          testCase "401 Unauthorized → ResponseError" $
            classifyResponse status401 @?= ResponseError,
          testCase "403 Forbidden → ResponseError" $
            classifyResponse status403 @?= ResponseError
        ],
      testGroup
        "urgencyFrom"
        [ testCase "LowPriority → 'low'" $
            urgencyFrom LowPriority @?= "low",
          testCase "HighPriority → 'high'" $
            urgencyFrom HighPriority @?= "high"
        ],
      testGroup
        "ttlFrom"
        [ testCase "Nothing → 0 (transient)" $
            ttlFrom Nothing @?= 0,
          testCase "Just 0 → 0" $
            ttlFrom (Just 0) @?= 0,
          testCase "Just 3600 → 3600" $
            ttlFrom (Just 3600) @?= 3600,
          testCase "Just maxBound → maxBound" $
            ttlFrom (Just maxBound) @?= maxBound
        ],
      testGroup
        "endpointOrigin"
        [ testCase "strips path, keeps origin" $
            endpointOrigin (ep "https://fcm.googleapis.com/fcm/send/abc")
              @?= "https://fcm.googleapis.com",
          testCase "preserves port" $
            endpointOrigin (ep "https://push.example.com:8443/webpush/abc")
              @?= "https://push.example.com:8443",
          testCase "handles no path" $
            endpointOrigin (ep "https://push.example.com")
              @?= "https://push.example.com",
          testCase "strips query string" $
            endpointOrigin (ep "https://push.example.com/wp?foo=bar")
              @?= "https://push.example.com",
          testCase "strips fragment" $
            endpointOrigin (ep "https://push.example.com/wp#/frag")
              @?= "https://push.example.com",
          testCase "strips query string when no path" $
            endpointOrigin (ep "https://push.example.com?foo=bar")
              @?= "https://push.example.com"
        ]
    ]

-- | Build an 'EndpointUrl', failing loudly if the URL is rejected. All test
-- URLs here are HTTPS (the production smart constructor enforces it), so a
-- 'Left' indicates a broken fixture.
ep :: Text -> EndpointUrl
ep raw =
  case mkEndpointUrl raw of
    Right e -> e
    Left e -> error ("WebPushDispatch.ep: invalid fixture URL " <> show raw <> ": " <> e)
