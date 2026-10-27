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
import qualified API.BrigCommon as BrigCommon
import qualified API.BrigInternal as BrigInternal
import API.Common (defPassword, randomEmail)
import qualified API.Galley as Galley
import qualified API.Spar as Spar
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types.URI as HttpUri
import qualified Network.URI as NetUri
import SetupHelpers
import qualified Test.LegalHold as LegalHold
import qualified Test.OAuth as OAuth
import Testlib.MockIntegrationService
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
testA0LoginWithValidTotpCode = for_ (catMaybes mockSecondFactors) $ \code -> do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  bindResponse (Brig.loginWithTotp alice email defPassword code) $ \resp ->
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
-- Other request bodies: parsing is real, enforcement is deliberately absent.
-- Each successful request gets fresh state because many of these are destructive.

mockSecondFactors :: [Maybe String]
mockSecondFactors =
  [ Nothing,
    Just mockLoginTotpCode,
    Just "recovery:3MK7-9WQX-0V2H-8TZB"
  ]

withSecondFactor :: Maybe String -> [Pair] -> Value
withSecondFactor code fields =
  object (fields <> ["totp_code" .= c | c <- toList code])

-- Do not accept a 4xx from business logic as proof that parsing worked: these
-- must all fail with 400 before a valid request can mutate the same resource.
assertMalformedSecondFactors :: (HasCallStack) => Value -> (Value -> App Response) -> App ()
assertMalformedSecondFactors body request =
  for_ [toJSON ("123456" :: String), toJSON ("totp:abc" :: String), toJSON ("recovery:invalid" :: String), Number 123456, Bool True] $ \code ->
    setField "totp_code" code body >>= request >>= assertStatus 400

assertPasswordStillRequired :: (HasCallStack) => String -> Value -> (Value -> App Response) -> App ()
assertPasswordStillRequired field body request =
  setField field ("wrong-password" :: String) body >>= request >>= assertStatus 403

testMockTotpDeleteSelf :: (HasCallStack) => App ()
testMockTotpDeleteSelf = for_ mockSecondFactors $ \code -> do
  alice <- randomUser OwnDomain def
  let body = withSecondFactor code ["password" .= defPassword]
      request = Brig.deleteSelfWithBody alice
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "password" body request
  request body >>= assertStatus 200

testMockTotpChangePassword :: (HasCallStack) => App ()
testMockTotpChangePassword = for_ mockSecondFactors $ \code -> do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  let newPassword = "new-test-password"
      body = withSecondFactor code ["old_password" .= defPassword, "new_password" .= newPassword]
      request = Brig.putPasswordWithBody alice
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "old_password" body request
  request body >>= assertStatus 200
  Brig.login alice email newPassword >>= assertStatus 200

testMockTotpClients :: (HasCallStack) => App ()
testMockTotpClients = for_ [6, 7, 8] $ \version -> withAPIVersion version
  $ for_ mockSecondFactors
  $ \code -> do
    alice <- randomUser OwnDomain def
    -- Adding a further permanent client requires the password, unlike the first.
    Brig.addClient alice def >>= assertStatus 201
    fields <- BrigCommon.mkAddClientValue def
    let body = withSecondFactor code fields
        add = Brig.addClientWithBody alice
    assertMalformedSecondFactors body add
    assertPasswordStillRequired "password" body add
    cid <- add body >>= getJSON 201 >>= objId
    let deleteBody = withSecondFactor code ["password" .= defPassword]
        remove = Brig.deleteClientWithBody alice cid
    assertMalformedSecondFactors deleteBody remove
    assertPasswordStillRequired "password" deleteBody remove
    remove deleteBody >>= assertStatus 200
    Brig.getClient alice cid >>= assertStatus 404

testMockTotpEmailUpdate :: (HasCallStack) => App ()
testMockTotpEmailUpdate = for_ mockSecondFactors $ \code -> do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  loginResponse <- Brig.login alice email defPassword
  loginResponse.status `shouldMatchInt` 200
  cookie <- assertJust "login cookie" (getCookie "zuid" loginResponse)
  token <- asString (loginResponse.json %. "access_token")
  newEmail <- randomEmail
  let body = withSecondFactor code ["email" .= newEmail]
      request = Brig.updateEmailWithBody alice ("zuid=" <> cookie) token
  assertMalformedSecondFactors body request
  request body >>= assertSuccess
  BrigInternal.getActivationCode alice newEmail >>= assertStatus 200

testMockTotpPasswordReset :: (HasCallStack) => App ()
testMockTotpPasswordReset = for_ mockSecondFactors $ \code -> do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  Brig.passwordReset alice email >>= assertSuccess
  reset <- BrigInternal.getPasswordResetCode alice email >>= getJSON 200
  key <- asString (reset %. "key")
  resetCode <- asString (reset %. "code")
  let newPassword = "new-test-password"
      body = withSecondFactor code ["key" .= key, "code" .= resetCode, "password" .= newPassword]
      request = Brig.completePasswordResetWithBody alice
  assertMalformedSecondFactors body request
  setField "code" ("wrong-code" :: String) body >>= request >>= assertStatus 400
  request body >>= assertStatus 200
  Brig.login alice email newPassword >>= assertStatus 200

testMockTotpAcceptInvitation :: (HasCallStack) => App ()
testMockTotpAcceptInvitation = for_ mockSecondFactors $ \code -> do
  (owner, tid, _) <- createTeam OwnDomain 1
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  invitation <- Brig.postInvitation owner (Brig.PostInvitation (Just email) Nothing) >>= getJSON 201
  invitationCode <- BrigInternal.getInvitationCode owner invitation >>= getJSON 200 >>= (%. "code") >>= asString
  let body = withSecondFactor code ["code" .= invitationCode, "password" .= defPassword]
      request = Brig.acceptTeamInvitationWithBody alice
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "password" body request
  request body >>= assertStatus 200
  self <- Brig.getSelf alice >>= getJSON 200
  self %. "team" `shouldMatch` tid

testMockTotpCreateApp :: (HasCallStack) => App ()
testMockTotpCreateApp = for_ mockSecondFactors $ \code -> do
  (owner, tid, _) <- createTeam OwnDomain 1
  let body =
        withSecondFactor
          code
          [ "name" .= ("test app" :: String),
            "category" .= ("other" :: String),
            "description" .= ("TOTP mock test" :: String),
            "password" .= defPassword
          ]
      request = Brig.createAppWithBody owner tid
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "password" body request
  request body >>= assertStatus 200

testMockTotpReauthenticate :: (HasCallStack) => App ()
testMockTotpReauthenticate = for_ mockSecondFactors $ \code -> do
  alice <- randomUser OwnDomain def
  let body = withSecondFactor code ["password" .= defPassword]
      request = BrigInternal.reauthenticateWithBody alice
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "password" body request
  request body >>= assertStatus 200

testMockTotpOAuthSessions :: (HasCallStack) => App ()
testMockTotpOAuthSessions = for_ [False, True] $ \singleSession ->
  for_ mockSecondFactors $ \code -> do
    alice <- randomUser OwnDomain def
    client <- OAuth.oauthClient alice
    cid <- asString client
    void $ OAuth.generateAccessToken alice client ["write-only:conversations"] OAuth.redirectUri
    [application] <- Brig.getOAuthApplications alice >>= getJSON 200 >>= asList
    [session] <- application %. "sessions" >>= asList
    tokenId <- asString (session %. "refresh_token_id")
    let body = withSecondFactor code ["password" .= defPassword]
        request = Brig.deleteOAuthSessionsWithBody alice cid (if singleSession then Just tokenId else Nothing)
    assertMalformedSecondFactors body request
    assertPasswordStillRequired "password" body request
    request body >>= assertSuccess
    apps <- Brig.getOAuthApplications alice >>= getJSON 200 >>= asList
    length apps `shouldMatchInt` 0

testMockTotpDeleteTeam :: (HasCallStack) => App ()
testMockTotpDeleteTeam = for_ mockSecondFactors $ \code -> do
  (owner, tid, _) <- createTeam OwnDomain 1
  let body = withSecondFactor code ["password" .= defPassword]
      request = Galley.deleteTeamWithBody owner tid
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "password" body request
  request body >>= assertStatus 202

testMockTotpDeleteTeamMember :: (HasCallStack) => App ()
testMockTotpDeleteTeamMember = for_ mockSecondFactors $ \code -> do
  (owner, tid, [mem]) <- createTeam OwnDomain 2
  uid <- objId mem
  let body = withSecondFactor code ["password" .= defPassword]
      request = Galley.deleteTeamMemberWithBody owner tid uid
  assertMalformedSecondFactors body request
  assertPasswordStillRequired "password" body request
  request body >>= assertSuccess

testMockTotpScimToken :: (HasCallStack) => App ()
testMockTotpScimToken = for_ [7, 8] $ \version -> withAPIVersion version
  $ for_ mockSecondFactors
  $ \code -> do
    (owner, _, _) <- createTeam OwnDomain 1
    let body = withSecondFactor code ["description" .= ("TOTP mock test" :: String), "password" .= defPassword]
        request = Spar.createScimTokenWithBody owner
    assertMalformedSecondFactors body request
    assertPasswordStillRequired "password" body request
    request body >>= assertStatus 200

testMockTotpLegalHold :: (HasCallStack) => App ()
-- Exercise approval, disabling, and settings deletion in lifecycle order.
-- Use explicit consent because implicit-consent mode forbids settings deletion.
testMockTotpLegalHold = LegalHold.ensureLHFeatureConfigForServer LegalHold.Explicit $ \domain ->
  for_ mockSecondFactors $ \secondFactor -> do
    -- Set up a team and a pending legal-hold device for its member.
    (owner, teamId, [mem]) <- createTeam domain 2
    void $ Galley.getSelfConversation owner
    void $ Galley.getSelfConversation mem
    LegalHold.whitelistOrEnableLHForTeam LegalHold.Explicit domain teamId owner
    withMockServer def (lhMockAppV V1) $ \address _ -> do
      Galley.postLegalHoldSettings teamId owner (mkLegalHoldSettings address) >>= assertStatus 201
      Galley.consentToLegalHold teamId mem defPassword >>= assertStatus 201
      Galley.requestLegalHoldDevice teamId owner mem >>= assertStatus 201

      -- The member approves their device. TOTP is parsed, but not enforced;
      -- a valid password is still required. Check failures before changing state.
      let body = withSecondFactor secondFactor ["password" .= defPassword]

          approveDevice = Galley.approveLegalHoldWithBody mem teamId
      assertMalformedSecondFactors body approveDevice
      assertPasswordStillRequired "password" body approveDevice
      approveDevice body >>= assertStatus 200

      -- The owner disables legal hold for the member, with the same auth checks.
      memberId <- objId mem
      let disableLegalHold = Galley.disableLegalHoldWithBody owner teamId memberId
      assertMalformedSecondFactors body disableLegalHold
      assertPasswordStillRequired "password" body disableLegalHold
      disableLegalHold body >>= assertSuccess

      -- Finally, the owner deletes the team's legal-hold settings.
      let deleteSettings = Galley.deleteLegalHoldSettingsWithBody owner teamId
      assertMalformedSecondFactors body deleteSettings
      assertPasswordStillRequired "password" body deleteSettings
      deleteSettings body >>= assertStatus 204

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
