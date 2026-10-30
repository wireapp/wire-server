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

module Test.TOTP where

import qualified API.Brig as Brig
import API.Common (defPassword)
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types.URI as HttpUri
import qualified Network.URI as NetUri
import SetupHelpers
import Testlib.Prelude

--------------------------------------------------------------------------------
-- mock bodies

mockEnrolBody :: Value
mockEnrolBody =
  object
    [ "name" .= ("work phone" :: String),
      "client_time" .= ("2026-02-03T11:17:05.021Z" :: String),
      "password" .= defPassword
    ]

mockActivateBody :: Value
mockActivateBody =
  object
    [ "code" .= ("014921" :: String),
      "client_time" .= ("2026-02-03T11:18:44.903Z" :: String)
    ]

mockRenameBody :: Value
mockRenameBody = object ["name" .= ("private phone" :: String)]

mockDeleteBody :: Value
mockDeleteBody = object ["password" .= defPassword]

mockLoginTotpCode :: String
mockLoginTotpCode = "totp:014921"

--------------------------------------------------------------------------------
-- POST /self/totp

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

--------------------------------------------------------------------------------
-- the other four routes

testTotpActivate :: (HasCallStack) => App ()
testTotpActivate = do
  alice <- randomUser OwnDomain def
  cid <- someCredentialId alice
  bindResponse (Brig.activateTotp alice cid mockActivateBody) $ \resp -> do
    resp.status `shouldMatchInt` 200
    codes <- resp.json %. "codes" & asList
    for_ codes $ \c -> do
      code <- asString c
      assertBool
        ("recovery code has the wrong shape: " <> code)
        (isRecoveryCodeShaped code)

testTotpList :: (HasCallStack) => App ()
testTotpList = do
  alice <- randomUser OwnDomain def
  bindResponse (Brig.listTotp alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    creds <- asList resp.json
    for_ creds $ \c -> do
      void $ asString (c %. "id")
      void $ asString (c %. "name")
      void $ asString (c %. "created_at")
      -- the secret and the URI are returned by enrolment and never again
      lookupField c "secret" `shouldMatch` noValue
      lookupField c "uri" `shouldMatch` noValue

testTotpRename :: (HasCallStack) => App ()
testTotpRename = do
  alice <- randomUser OwnDomain def
  cid <- someCredentialId alice
  bindResponse (Brig.renameTotp alice cid mockRenameBody) $ \resp ->
    resp.status `shouldMatchInt` 200

testTotpDelete :: (HasCallStack) => App ()
testTotpDelete = do
  alice <- randomUser OwnDomain def
  cid <- someCredentialId alice
  bindResponse (Brig.deleteTotp alice cid mockDeleteBody) $ \resp ->
    resp.status `shouldMatchInt` 200

--------------------------------------------------------------------------------
-- version gate

testTotpNotOnV18 :: (HasCallStack) => App ()
testTotpNotOnV18 = do
  alice <- randomUser OwnDomain def
  let routes =
        [ ("POST", "/self/totp"),
          ("GET", "/self/totp"),
          ("POST", "/self/totp/" <> credentialIdPlaceholder <> "/activate"),
          ("PUT", "/self/totp/" <> credentialIdPlaceholder),
          ("DELETE", "/self/totp/" <> credentialIdPlaceholder)
        ]
  for_ routes $ \(method, path) -> do
    req <- baseRequest alice Brig (ExplicitVersion 18) path
    bindResponse (submit method (req & addJSONObject ["name" .= ("work phone" :: String)])) $ \resp ->
      resp.status `shouldMatchOneOf` [Number 404, Number 405]

--------------------------------------------------------------------------------
-- nginz

testTotpThroughNginz :: (HasCallStack) => App ()
testTotpThroughNginz = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  token <- do
    resp <- Brig.login alice email defPassword >>= getJSON 200
    asString (resp %. "access_token")

  -- authenticated: the request reaches brig
  do
    req <-
      rawBaseRequest alice Nginz Versioned "/self/totp"
        <&> addHeader "Authorization" ("Bearer " <> token)
    bindResponse (submit "GET" req) $ \resp -> do
      resp.status `shouldMatchInt` 200
      void $ asList resp.json

  -- a bad token: nginz rejects it before brig sees anything
  do
    req <-
      rawBaseRequest alice Nginz Versioned "/self/totp"
        <&> addHeader "Authorization" "Bearer not-a-token"
    bindResponse (submit "GET" req) $ \resp ->
      resp.status `shouldMatchInt` 401

  -- no credentials at all
  do
    req <- rawBaseRequest alice Nginz Versioned "/self/totp"
    bindResponse (submit "GET" req) $ \resp ->
      assertBool
        ("unauthenticated request was not rejected: " <> show resp.status)
        (resp.status >= 400 && resp.status < 500)

--------------------------------------------------------------------------------
-- mock login with TOTP code

-- | Login with a totp_code field succeeds: the mock parses the field and
-- ignores it.
testA0LoginWithValidTotpCode :: (HasCallStack) => App ()
testA0LoginWithValidTotpCode = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  bindResponse (Brig.loginWithTotp alice email defPassword mockLoginTotpCode) $ \resp ->
    resp.status `shouldMatchInt` 200

-- | Login with a malformed totp_code value (not a valid SecondFactorCode
-- format) returns 400 — parsing is real even though enforcement is not.
testA0LoginWithInvalidTotpCode :: (HasCallStack) => App ()
testA0LoginWithInvalidTotpCode = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  req <- baseRequest alice Brig Versioned "login"
  bindResponse
    ( submit "POST"
        $ req
        & addJSONObject ["email" .= email, "password" .= defPassword, "totp_code" .= ("not-a-valid-totp-code" :: String)]
        & addQueryParams [("persist", "true")]
    )
    $ \resp ->
      resp.status `shouldMatchInt` 400

--------------------------------------------------------------------------------
-- helpers

someCredentialId :: (HasCallStack, MakesValue user) => user -> App String
someCredentialId user = do
  resp <- Brig.enrolTotp user mockEnrolBody >>= getJSON 201
  asString (resp %. "credential_id")

credentialIdPlaceholder :: String
credentialIdPlaceholder = "0f2e1b9c-6c6c-4a0e-9a3f-17a2cf15a001"

responseHeader :: String -> Response -> Maybe ByteString
responseHeader name resp =
  snd <$> find ((CI.mk (BSChar8.pack name) ==) . fst) resp.headers

-- | @xxxx-xxxx-xxxx-xxxx@ in Crockford base32 (no I, L, O, U).
isRecoveryCodeShaped :: String -> Bool
isRecoveryCodeShaped s =
  (length groups == 4)
    && all ((== 4) . length) groups
    && all (all (`elem` crockford)) groups
  where
    crockford :: String
    crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

    groups = splitOnDash s
    splitOnDash t = case break (== '-') t of
      (g, []) -> [g]
      (g, _ : rest) -> g : splitOnDash rest
