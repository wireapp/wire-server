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

module Test.TOTP.Enrol where

import qualified API.Brig as Brig
import qualified Data.ByteString.Char8 as BSChar8
import qualified Network.HTTP.Types.URI as HttpUri
import qualified Network.URI as NetUri
import SetupHelpers
import Test.TOTP.Helpers (responseHeader)
import Test.TOTP.MockBodies
import Testlib.Prelude

testTotpEnrol :: (HasCallStack) => App ()
testTotpEnrol = do
  alice <- randomUser OwnDomain def
  bindResponse (Brig.enrolTotp alice mockEnrolBody) $ \resp -> do
    resp.status `shouldMatchInt` 201

    -- the one response in the set that carries a secret must not be cached
    let cacheControl = map toLower . BSChar8.unpack <$> responseHeader "Cache-Control" resp
    assertBool
      ("expected Cache-Control: no-store, got " <> show cacheControl)
      (cacheControl == Just "no-store")

    void $ asString (resp.json %. "credential_id")
    void $ asString (resp.json %. "secret")
    void $ asString (resp.json %. "uri")
    alg <- asString (resp.json %. "algorithm")
    digits <- asInt (resp.json %. "digits")
    period <- asInt (resp.json %. "period")
    void $ asString (resp.json %. "server_time")
    void $ asString (resp.json %. "expires_at")

    -- the mock pins these rather than drawing them at random, so that a client
    -- that hardcodes the defaults does not look broken against it
    alg `shouldMatch` "sha1"
    digits `shouldMatchInt` 6
    period `shouldMatchInt` 30

-- | secret and uri field in response match.
testTotpEnrolUriMatchesSecret :: (HasCallStack) => App ()
testTotpEnrolUriMatchesSecret = do
  alice <- randomUser OwnDomain def
  bindResponse (Brig.enrolTotp alice mockEnrolBody) $ \resp -> do
    resp.status `shouldMatchInt` 201
    secret <- asString (resp.json %. "secret")
    uriText <- asString (resp.json %. "uri")

    uri <- case NetUri.parseURI uriText of
      Nothing -> assertFailure ("uri does not parse as a URI: " <> uriText)
      Just u -> pure u
    NetUri.uriScheme uri `shouldMatch` "otpauth:"
    case NetUri.uriAuthority uri of
      Just auth -> NetUri.uriRegName auth `shouldMatch` "totp"
      Nothing -> assertFailure ("uri has no authority: " <> uriText)

    let params = HttpUri.parseQuery (BSChar8.pack (NetUri.uriQuery uri))
        param k =
          maybe
            (assertFailure ("uri has no " <> k <> " parameter: " <> uriText))
            (pure . BSChar8.unpack)
            (join (lookup (BSChar8.pack k) params))
    -- this is the assertion the whole test exists for
    param "secret" `shouldMatch` secret
    param "issuer" `shouldMatch` "Wire"
    param "algorithm" `shouldMatch` "SHA1"
    param "digits" `shouldMatch` "6"
    param "period" `shouldMatch` "30"
