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

-- | Unit tests for "Gundeck.Push.Web.Ssrf".
module WebPushSsrf
  ( tests,
  )
where

import Data.ByteString.Char8 qualified as BS
import Gundeck.Push.Web.Ssrf
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "WebPushSsrf"
    [ testGroup
        "isPrivateLiteralHost — IPv4 private ranges"
        [ testCase "rejects 10.0.0.1 (RFC 1918)" $
            isPrivate "10.0.0.1",
          testCase "rejects 172.16.0.1 (RFC 1918)" $
            isPrivate "172.16.0.1",
          testCase "rejects 172.31.255.255 (RFC 1918 upper bound)" $
            isPrivate "172.31.255.255",
          testCase "rejects 192.168.1.1 (RFC 1918)" $
            isPrivate "192.168.1.1",
          testCase "rejects 127.0.0.1 (loopback)" $
            isPrivate "127.0.0.1",
          testCase "rejects 127.1.2.3 (loopback, non-0.0.1)" $
            isPrivate "127.1.2.3",
          testCase "rejects 0.0.0.0 (this network)" $
            isPrivate "0.0.0.0",
          testCase "rejects 169.254.169.254 (AWS metadata)" $
            isPrivate "169.254.169.254",
          testCase "rejects 169.254.0.1 (link-local)" $
            isPrivate "169.254.0.1",
          testCase "rejects 100.64.0.1 (CGNAT)" $
            isPrivate "100.64.0.1"
        ],
      testGroup
        "isPrivateLiteralHost — IPv6 private ranges"
        [ testCase "rejects ::1 (loopback)" $
            isPrivate "::1",
          testCase "rejects :: (unspecified)" $
            isPrivate "::",
          testCase "rejects fc00::1 (unique local)" $
            isPrivate "fc00::1",
          testCase "rejects fd12:3456:789a::1 (unique local)" $
            isPrivate "fd12:3456:789a::1",
          testCase "rejects fe80::1 (link-local)" $
            isPrivate "fe80::1",
          testCase "rejects ::ffff:127.0.0.1 (IPv4-mapped loopback)" $
            isPrivate "::ffff:127.0.0.1",
          testCase "rejects ::ffff:169.254.169.254 (IPv4-mapped metadata)" $
            isPrivate "::ffff:169.254.169.254"
        ],
      testGroup
        "isPrivateLiteralHost — hostname and public IPs"
        [ testCase "rejects 'localhost'" $
            isPrivate "localhost",
          testCase "rejects 'LOCALHOST' (case-insensitive)" $
            isPrivate "LOCALHOST",
          testCase "accepts a public IPv4" $
            isNotPrivate "142.250.190.46",
          testCase "accepts 172.32.0.1 (just outside RFC 1918 172.16/12)" $
            -- 172.32.0.0/16 is NOT in 172.16.0.0/12 (which covers 172.16–172.31).
            isNotPrivate "172.32.0.1",
          testCase "accepts a public IPv6" $
            isNotPrivate "2606:4700:4700::1111",
          testCase "accepts a hostname (not a literal IP)" $
            isNotPrivate "fcm.googleapis.com",
          testCase "accepts a hostname with subdomain" $
            isNotPrivate "updates.push.services.mozilla.com",
          testCase "treats hex-encoded IP as hostname (not literal)" $
            -- 0x7f000001 is 127.0.0.1 in hex; parseIPv4 rejects it (single
            -- dot-group), so it is treated as a hostname and subject to the
            -- registration allowlist rather than the literal-IP filter.
            isNotPrivate "0x7f000001",
          testCase "treats decimal-encoded IP as hostname" $
            isNotPrivate "2130706433",
          testCase "treats FQDN 'localhost.' as a hostname" $
            isNotPrivate "localhost."
        ]
    ]

isPrivate :: [Char] -> Assertion
isPrivate s = assertBool ("expected private: " <> s) (isPrivateLiteralHost (BS.pack s))

isNotPrivate :: [Char] -> Assertion
isNotPrivate s = assertBool ("expected NOT private: " <> s) (not (isPrivateLiteralHost (BS.pack s)))
