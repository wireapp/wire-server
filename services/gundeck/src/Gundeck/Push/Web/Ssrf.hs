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

-- | Pure SSRF egress filter for the web push hardened HTTP 'Manager'.
--
-- 'isPrivateLiteralHost' inspects a request's host string and reports whether
-- it is a /literal/ IP address in a private, loopback, link-local, or
-- unspecified range — i.e. an address gundeck must never POST a web push
-- notification to (RFC 1918, RFC 3927, RFC 4193, RFC 6598, RFC 4291).
--
-- == Scope and limits (v1)
--
-- This is a *literal* check only. It catches the obvious direct-literal cases:
-- @http://127.0.0.1\/...@, @http:\/\/169.254.169.254\/...@ (cloud metadata),
-- @http:\/\/[::1]\/...@, etc. It does /not/ catch:
--
-- * DNS rebinding, where an allowlisted hostname resolves to a private IP at
--   request time. The registration-time @_endpointAllowlist@
--   ('Gundeck.Options.endpointAllowlist') is the primary mitigation; this
--   module is belt-and-suspenders.
-- * Hostnames that are not literal IPs. Such strings return 'False' here — the
--   host allowlist handles them.
--
-- Full DNS-resolution-time filtering is a deliberate v1 follow-up: it requires
-- an @getaddrinfo@ lookup at the @managerModifyRequest@ layer, which adds IO
-- and failure modes.
module Gundeck.Push.Web.Ssrf
  ( isPrivateLiteralHost,
    isPrivateIPv4,
    parseIPv4,
  )
where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString.Char8 qualified as BC
import Imports

-- | Returns 'True' if the host is a literal IP address in a private,
-- loopback, link-local, cloud-metadata, CGNAT, or unspecified range.
--
-- The conventional loopback hostname @"localhost"@ is also treated as
-- private. All other hostnames (non-IP strings) return 'False'.
isPrivateLiteralHost :: ByteString -> Bool
isPrivateLiteralHost h
  | BC.map toLower h == "localhost" = True
  | Just w <- parseIPv4 h = isPrivateIPv4 w
  | otherwise = isPrivateIPv6Literal h

--------------------------------------------------------------------------------
-- IPv4

-- | Parse a dotted-decimal IPv4 literal (four octets, 0–255 each) into a
-- big-endian 'Word32'. Returns 'Nothing' for anything that is not exactly
-- four dot-separated decimal octets in range — including hostnames, IPv6
-- literals, and malformed addresses.
--
-- Leading zeros are accepted and parsed as decimal (i.e. @010.0.0.1@ becomes
-- @10.0.0.1@, /not/ octal); this matches what a browser would resolve.
parseIPv4 :: ByteString -> Maybe Word32
parseIPv4 bs =
  case BC.split '.' bs of
    [a, b, c, d] -> do
      a' <- readOctet a
      b' <- readOctet b
      c' <- readOctet c
      d' <- readOctet d
      Just $ (a' `shiftL` 24) .|. (b' `shiftL` 16) .|. (c' `shiftL` 8) .|. d'
    _ -> Nothing
  where
    readOctet :: ByteString -> Maybe Word32
    readOctet s =
      case readMaybe (BC.unpack s) of
        Just n | n <= 255 -> Just n
        _ -> Nothing

-- | IPv4 private \/ reserved ranges:
--
-- * @0.0.0.0\/8@   — "this network" (RFC 1122)
-- * @10.0.0.0\/8@   — private (RFC 1918)
-- * @100.64.0.0\/10@ — CGNAT (RFC 6598)
-- * @127.0.0.0\/8@  — loopback
-- * @169.254.0.0\/16@ — link-local (RFC 3927); includes the AWS / GCP cloud
--   metadata endpoints at @169.254.169.254@, the canonical SSRF target.
-- * @172.16.0.0\/12@ — private (RFC 1918)
-- * @192.168.0.0\/16@ — private (RFC 1918)
isPrivateIPv4 :: Word32 -> Bool
isPrivateIPv4 w =
  (w .&. 0xFF000000) == 0x00000000
    || (w .&. 0xFF000000) == 0x0A000000
    || (w .&. 0xFFC00000) == 0x64400000
    || (w .&. 0xFF000000) == 0x7F000000
    || (w .&. 0xFFFF0000) == 0xA9FE0000
    || (w .&. 0xFFF00000) == 0xAC100000
    || (w .&. 0xFFFF0000) == 0xC0A80000

--------------------------------------------------------------------------------
-- IPv6 (coarse literal-prefix matching for the common private ranges).
-- Full RFC 4291 parsing with '::' expansion is out of scope for v1.

isPrivateIPv6Literal :: ByteString -> Bool
isPrivateIPv6Literal raw
  -- An IPv6 literal always contains at least one ':'. Rejecting strings
  -- without ':' here avoids false positives on hostnames that happen to
  -- start with "fc" or "fd" (e.g. "fcm.googleapis.com").
  | not (BC.elem ':' h) = False
  | otherwise =
      -- IPv4-mapped (::ffff:a.b.c.d): the embedded IPv4 carries the real
      -- destination. Strip the prefix and re-check the IPv4 part.
      case BC.stripPrefix "::ffff:" h of
        Just rest -> isJust (parseIPv4 rest)
        Nothing ->
          h == "::1"
            || h == "::"
            || h == "::0"
            || "fc" `BC.isPrefixOf` h
            || "fd" `BC.isPrefixOf` h
            || "fe80:" `BC.isPrefixOf` h
            || "fe90:" `BC.isPrefixOf` h
            || "fea0:" `BC.isPrefixOf` h
            || "feb0:" `BC.isPrefixOf` h
  where
    h = BC.map toLower raw
