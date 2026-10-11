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
--
-- The handler-specific, security-critical logic that lives in 'Gundeck.Push'
-- is the SSRF validation around the attacker-controllable @endpoint@ URL
-- ('validateEndpointHost' / 'endpointHost'): gundeck will later POST to that
-- URL. These tests cover that logic directly.
--
-- The store-level acceptance criteria (upsert on re-register, delete-by-row)
-- are exercised against the in-memory interpreter and Postgres in
-- @wire-subsystems@; the full 'Gundeck.Gundeck'-monad handlers sit on top of
-- 'Gundeck.Push.Web.Runner.runWebPush', which needs a live pool and is
-- therefore covered by integration rather than unit tests.
module WebPushHandlers where

import Control.Lens ((^.))
import Data.ByteString qualified as BS
import Data.Id
import Gundeck.Push
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Push.V2
import Wire.API.Push.V2.WebSubscription (mkEndpointUrl)
import Wire.WebPushStore (WebPushAddress (..))

tests :: TestTree
tests =
  testGroup
    "WebPushHandlers"
    [ testGroup
        "validateEndpointHost"
        [ testCase "empty allowlist accepts any endpoint" $
            isAccepted $
              validateEndpointHost [] (ep "https://fcm.googleapis.com/fcm/send/abc"),
          testCase "exact host match is accepted" $
            isAccepted $
              validateEndpointHost ["fcm.googleapis.com"] (ep "https://fcm.googleapis.com/fcm/send/abc"),
          testCase "subdomain of an allowed host is accepted" $
            isAccepted $
              validateEndpointHost ["googleapis.com"] (ep "https://fcm.googleapis.com/fcm/send/abc"),
          testCase "host not on the allowlist is rejected" $
            isRejected $
              validateEndpointHost ["fcm.googleapis.com"] (ep "https://example.com/push/abc"),
          testCase "SSRF: suffix entry must not match an unrelated host" $
            -- 'evil.com' must NOT match 'notevil.com'. A naive Text.isSuffixOf
            -- over the URL would wrongly accept this.
            isRejected $
              validateEndpointHost ["evil.com"] (ep "https://notevil.com/push"),
          testCase "SSRF: allowed host as a prefix-of-host is rejected" $
            -- 'example.com' must NOT match 'example.com.attacker.tld'. A naive
            -- suffix check over the host string (without a dot boundary) would
            -- wrongly accept this.
            isRejected $
              validateEndpointHost ["example.com"] (ep "https://example.com.attacker.tld/push"),
          testCase "SSRF: deeper subdomain of an allowed host is accepted" $
            isAccepted $
              validateEndpointHost ["googleapis.com"] (ep "https://fcm-x.googleapis.com/x"),
          testCase "SSRF: userinfo must not fool the host check" $
            -- @https://allowed.com@evil.com/@ has userinfo 'allowed.com' but
            -- the real destination host is 'evil.com'. Since we extract the
            -- authority host (not the userinfo), this must be rejected.
            isRejected $
              validateEndpointHost ["allowed.com"] (ep "https://allowed.com@evil.com/push"),
          testCase "host matching is case-insensitive (RFC 3986)" $
            isAccepted $
              validateEndpointHost ["FCM.GoogleAPIs.COM"] (ep "https://fcm.googleapis.com/x"),
          testCase "multiple allowlist entries: any match suffices" $
            isAccepted $
              validateEndpointHost ["fcm.googleapis.com", "updates.push.services.mozilla.com"] (ep "https://updates.push.services.mozilla.com/wpush/v2/xyz")
        ],
      testGroup
        "endpointHost"
        [ testCase "extracts the host of a well-formed https URL" $
            endpointHost (ep "https://fcm.googleapis.com/fcm/send/abc")
              @?= Just "fcm.googleapis.com",
          testCase "handles a URL with userinfo and port" $
            endpointHost (ep "https://u:p@fcm.googleapis.com:443/x")
              @?= Just "fcm.googleapis.com"
        ],
      testCase "addressToSubscription preserves fields, reports expiration=Nothing" $ do
        let addr =
              WebPushAddress
                { wpaUser = read "00000000-0000-0001-0000-000000000000",
                  wpaConn = ConnId "conn",
                  wpaClient = ClientId 7,
                  wpaEndpoint = ep "https://fcm.googleapis.com/fcm/send/abc",
                  wpaKeys = keys
                }
            sub = addressToSubscription addr
        sub ^. wpsEndpoint @?= addr.wpaEndpoint
        sub ^. wpsKeys @?= addr.wpaKeys
        sub ^. wpsClient @?= addr.wpaClient
        sub ^. wpsExpirationTime @?= Nothing
    ]

-- | Assert that validation accepted the endpoint. 'AddWebPushError' has no
-- 'Eq' instance, so we pattern-match rather than using '@?='.
isAccepted :: Either AddWebPushError () -> Assertion
isAccepted (Right ()) = pure ()
isAccepted (Left e) = assertFailure ("expected acceptance (Right ()), got " <> show e)

-- | Assert that validation rejected the endpoint with 'AddWebPushErrorInvalid'.
isRejected :: Either AddWebPushError () -> Assertion
isRejected (Left AddWebPushErrorInvalid) = pure ()
isRejected (Left e) = assertFailure ("expected AddWebPushErrorInvalid, got " <> show e)
isRejected (Right ()) = assertFailure "expected rejection (Left AddWebPushErrorInvalid), got Right ()"

-- | Build an 'EndpointUrl', failing loudly if the URL is rejected. All test
-- URLs here must already be HTTPS (the production smart constructor enforces
-- HTTPS), so a 'Left' indicates a broken fixture rather than behaviour.
ep :: Text -> EndpointUrl
ep raw =
  case mkEndpointUrl raw of
    Right e -> e
    Left e -> error ("WebPushHandlers.ep: invalid fixture URL " <> show raw <> ": " <> e)

-- | A fixed, valid key pair (RFC 8291: 65-byte uncompressed P-256 public key
-- + 16-byte auth secret). Only the shape matters here, not the cryptographic
-- value, since 'addressToSubscription' only moves the fields around.
keys :: WebPushKeys
keys =
  WebPushKeys
    (P256dhKey (BS.replicate 65 0x04))
    (AuthSecret (BS.replicate 16 0x01))
