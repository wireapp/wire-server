{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Test.Login where

import qualified API.Brig as Brig
import API.BrigInternal (getVerificationCode)
import qualified API.BrigInternal as BrigInternal
import API.Common (defPassword)
import API.GalleyInternal
import API.Nginz (login, loginWith2ndFactor)
import qualified API.Nginz as Nginz
import qualified API.Spar as Spar
import Control.Concurrent (threadDelay)
import Control.Monad.Codensity (runCodensity)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Database.CQL.IO as Cassandra
import Database.CQL.Protocol (Blob)
import SetupHelpers
import qualified Testlib.PG as PG
import Testlib.Prelude
import Testlib.ResourcePool (acquireResources)
import Text.Printf (printf)

testLoginVerify6DigitEmailCodeSuccess :: (HasCallStack) => App ()
testLoginVerify6DigitEmailCodeSuccess = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  code <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
  bindResponse (loginWith2ndFactor owner email defPassword code) $ \resp -> do
    resp.status `shouldMatchInt` 200

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that login fails with wrong second factor email verification code
testLoginVerify6DigitWrongCodeFails :: (HasCallStack) => App ()
testLoginVerify6DigitWrongCodeFails = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  correctCode <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
  let wrongCode :: String = printf "%06d" $ (read @Int correctCode) + 1 `mod` 1000000
  bindResponse (loginWith2ndFactor owner email defPassword wrongCode) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that login without verification code fails if SndFactorPasswordChallenge feature is enabled in team
testLoginVerify6DigitMissingCodeFails :: (HasCallStack) => App ()
testLoginVerify6DigitMissingCodeFails = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  bindResponse (login owner email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-required"

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that login fails with expired second factor email verification code
testLoginVerify6DigitExpiredCodeFails :: (HasCallStack) => App ()
testLoginVerify6DigitExpiredCodeFails = do
  withModifiedBackend
    (def {brigCfg = setField "optSettings.setVerificationTimeout" (Aeson.Number 2)})
    $ \domain -> do
      (owner, team, []) <- createTeam domain 0
      email <- owner %. "email"
      setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
      assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
      bindResponse (getTeamFeature owner team "sndFactorPasswordChallenge") $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "enabled"
      generateVerificationCode owner email
      code <- bindResponse (getVerificationCode owner "login") $ \resp -> do
        resp.status `shouldMatchInt` 200
        asString resp.json
      liftIO $ threadDelay 2_000_100
      bindResponse (loginWith2ndFactor owner email defPassword code) \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- @END

testLoginVerify6DigitResendCodeSuccessAndRateLimiting :: (HasCallStack) => App ()
testLoginVerify6DigitResendCodeSuccessAndRateLimiting = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  fstCode <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
  bindResponse (generateVerificationCode' owner email) $ \resp -> do
    resp.status `shouldMatchInt` 429
  mostRecentCode <- retryT $ do
    resp <- generateVerificationCode' owner email
    resp.status `shouldMatchInt` 200
    getVerificationCode owner "login" >>= getJSON 200 >>= asString

  bindResponse (loginWith2ndFactor owner email defPassword fstCode) \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-failed"

  bindResponse (loginWith2ndFactor owner email defPassword mostRecentCode) \resp -> do
    resp.status `shouldMatchInt` 200

testLoginVerify6DigitLimitRetries :: (HasCallStack) => App ()
testLoginVerify6DigitLimitRetries = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  correctCode <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
  let wrongCode :: String = printf "%06d" $ (read @Int correctCode) + 1 `mod` 1000000
  -- try login with wrong code should fail 3 times
  forM_ [1 .. 3] $ \(_ :: Int) -> do
    bindResponse (loginWith2ndFactor owner email defPassword wrongCode) \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "code-authentication-failed"
  -- after 3 failed attempts, login with correct code should fail as well
  bindResponse (loginWith2ndFactor owner email defPassword correctCode) \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- Credential-policy regression tests; backend implementation is a separate change.
createSamlScimTokenForLoginTest :: (HasCallStack) => Value -> String -> App String
createSamlScimTokenForLoginTest owner team = do
  setTeamFeatureStatus owner team "mfa-totp" "disabled" >>= assertSuccess
  setTeamFeatureStatus owner team "sso" "enabled" >>= assertSuccess
  setTeamFeatureStatus owner team "validateSAMLemails" "enabled" >>= assertSuccess
  (idpResponse, _) <- registerTestIdPWithMetaWithPrivateCreds owner
  idp <- idpResponse & getJSON 201 >>= (%. "id") >>= asString
  Spar.createScimToken owner def {Spar.idp = Just idp} >>= getJSON 200 >>= (%. "token") >>= asString

adoptSamlUserForLoginTest :: (HasCallStack) => String -> Value -> App ()
adoptSamlUserForLoginTest token user = do
  uid <- objId user
  email <- user %. "email" >>= asString
  -- SCIM GET adopts the existing password account and assigns the token's IdP.
  Spar.getScimUser user token uid `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" `shouldMatch` uid
  profile <- Brig.getSelf user >>= getJSON 200
  profile %. "managed_by" `shouldMatch` "scim"
  subject <- profile %. "sso_id.subject" >>= asString
  subject `shouldContainString` email

createSamlScimUserForLoginTest :: (HasCallStack) => Value -> String -> App Value
createSamlScimUserForLoginTest owner token = do
  scimUser <- randomScimUser
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  uid <- Spar.createScimUser owner token scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  activateEmail OwnDomain email
  BrigInternal.getUsersId OwnDomain [uid] >>= getJSON 200 >>= asList >>= assertOne

testLoginSamlAssignmentDisablesPassword :: (HasCallStack) => App ()
testLoginSamlAssignmentDisablesPassword = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  token <- createSamlScimTokenForLoginTest owner team
  email <- user %. "email" >>= asString
  login user email defPassword >>= assertStatus 200
  adoptSamlUserForLoginTest token user
  login user email defPassword >>= assertStatus 403
  Brig.login user email defPassword >>= assertStatus 403
  -- Assignment is per account, not a team-wide password ban.
  ownerEmail <- owner %. "email"
  login owner ownerEmail defPassword >>= assertStatus 200

testLoginScimAdoptionWithoutIdpPreservesPassword :: (HasCallStack) => App ()
testLoginScimAdoptionWithoutIdpPreservesPassword = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  void $ createSamlScimTokenForLoginTest owner team
  token <- Spar.createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  uid <- objId user
  email <- user %. "email" >>= asString
  login user email defPassword >>= assertStatus 200
  Spar.getScimUser user token uid >>= assertStatus 200
  profile <- Brig.getSelf user >>= getJSON 200
  profile %. "managed_by" `shouldMatch` "scim"
  profile %. "sso_id.scim_external_id" `shouldMatch` email
  login user email defPassword >>= assertStatus 200
  Brig.login user email defPassword >>= assertStatus 200

testLoginSamlRejectsPasswordEvenWithEmailVerificationCode :: (HasCallStack) => App ()
testLoginSamlRejectsPasswordEvenWithEmailVerificationCode = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  token <- createSamlScimTokenForLoginTest owner team
  email <- user %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled" >>= assertSuccess
  -- The legacy email challenge is not an eligible Wire-local MFA factor.
  generateVerificationCode user email
  code <- getVerificationCode user "login" >>= getJSON 200 >>= asString
  adoptSamlUserForLoginTest token user
  loginWith2ndFactor user email defPassword code >>= assertStatus 403

testLoginSamlAssignmentRemovesPasswordHash :: (HasCallStack) => App ()
testLoginSamlAssignmentRemovesPasswordHash = do
  -- TODO: make this a unit test in /libs/wire-subsystems/
  pool <- asks (.resourcePool)
  for_ ["cassandra", "postgresql"] $ \storage ->
    runCodensity (acquireResources 1 pool) $ \[backend] -> do
      let configure = setField @_ @String "postgresMigration.user" storage
          overrides =
            def
              { brigCfg = configure,
                galleyCfg = configure,
                backgroundWorkerCfg = configure >=> setField "migrateUsers" False
              }
      runCodensity (startDynamicBackend backend overrides) $ \_ -> do
        let domain = backend.berDomain
        (owner, team, [user]) <- createTeam domain 2
        token <- createSamlScimTokenForLoginTest owner team
        Just uid <- UUID.fromString <$> objId user
        checkHash storage backend uid True
        adoptSamlUserForLoginTest token user
        checkHash storage backend uid False
  where
    checkHash :: String -> BackendResource -> UUID.UUID -> Bool -> App ()
    checkHash "cassandra" backend uid expected = do
      let selectPassword :: Cassandra.PrepQuery Cassandra.R (Identity UUID.UUID) (Identity (Maybe Blob))
          selectPassword = fromString $ "SELECT password FROM " <> backend.berBrigKeyspace <> ".user WHERE id = ?"
      row <- Cassandra.query1 selectPassword (Cassandra.defQueryParams Cassandra.LocalQuorum (Identity uid))
      Just (Identity password) <- pure row
      assertBool "Unexpected Cassandra password hash presence" (isJust password == expected)
    checkHash _ backend uid expected =
      -- UUID is parsed above; never interpolate an unvalidated value or print a hash.
      (PG.execSql backend . fromString)
        ( "DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM wire_user WHERE id = '"
            <> UUID.toString uid
            <> "' AND password IS "
            <> (if expected then "NOT NULL" else "NULL")
            <> ") THEN RAISE EXCEPTION 'Unexpected PostgreSQL password hash presence'; END IF; END $$;"
        )

testLoginSamlAssignmentPreservesSession :: (HasCallStack) => App ()
testLoginSamlAssignmentPreservesSession = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  scimToken <- createSamlScimTokenForLoginTest owner team
  email <- user %. "email" >>= asString
  (cookie, token) <- loginWithCookieAndToken user email defPassword
  adoptSamlUserForLoginTest scimToken user
  -- Exercise the gateway, not a direct request with a synthetic Z-User header.
  Nginz.getSelf user token >>= assertStatus 200
  Nginz.access user cookie >>= assertStatus 200

testLoginSamlAccountCannotSetPassword :: (HasCallStack) => App ()
testLoginSamlAccountCannotSetPassword = do
  (owner, team, []) <- createTeam OwnDomain 0
  token <- createSamlScimTokenForLoginTest owner team
  user <- createSamlScimUserForLoginTest owner token
  email <- user %. "email"
  let newPassword = "new-synthetic-password"
  Brig.putPasswordWithBody user (object ["new_password" .= newPassword]) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "saml-managed-user"
  login user email newPassword >>= assertStatus 403

testLoginSamlAssignmentDisablesPasswordChange :: (HasCallStack) => App ()
testLoginSamlAssignmentDisablesPasswordChange = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  token <- createSamlScimTokenForLoginTest owner team
  email <- user %. "email"
  login user email defPassword >>= assertStatus 200
  adoptSamlUserForLoginTest token user
  let newPassword = "new-synthetic-password"
  Brig.putPassword user defPassword newPassword `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "saml-managed-user"
  login user email newPassword >>= assertStatus 403

-- | Freshly provisioned accounts are covered by Test.Spar.testNoPasswordResetForSAMLUser.
testLoginSamlAssignmentDisablesPasswordReset :: (HasCallStack) => App ()
testLoginSamlAssignmentDisablesPasswordReset = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  token <- createSamlScimTokenForLoginTest owner team
  adoptSamlUserForLoginTest token user
  email <- user %. "email" >>= asString
  -- Preserve the non-enumerating public response, without generating a code.
  Brig.passwordReset user email >>= assertStatus 201
  BrigInternal.getPasswordResetCode user email `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-key"

testLoginSamlAssignmentInvalidatesPendingPasswordReset :: (HasCallStack) => App ()
testLoginSamlAssignmentInvalidatesPendingPasswordReset = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  token <- createSamlScimTokenForLoginTest owner team
  email <- user %. "email" >>= asString
  Brig.passwordReset user email >>= assertStatus 201
  reset <- BrigInternal.getPasswordResetCode user email >>= getJSON 200
  key <- reset %. "key" >>= asString
  code <- reset %. "code" >>= asString
  adoptSamlUserForLoginTest token user
  let newPassword = "new-synthetic-password"
  Brig.completePasswordReset user key code newPassword `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-code"
  login user email newPassword >>= assertStatus 403
  login user email defPassword >>= assertStatus 403

testLoginScimUnlinkRequiresPasswordRecovery :: (HasCallStack) => App ()
testLoginScimUnlinkRequiresPasswordRecovery = do
  (owner, team, [user]) <- createTeam OwnDomain 2
  samlToken <- createSamlScimTokenForLoginTest owner team
  uid <- objId user
  email <- user %. "email" >>= asString
  login user email defPassword >>= assertStatus 200
  adoptSamlUserForLoginTest samlToken user

  -- Bringing the same account under a SCIM token without an IdP unlinks SAML.
  scimOnlyToken <- Spar.createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  scimUser <- Spar.getScimUser OwnDomain scimOnlyToken uid >>= getJSON 200
  scimUser %. "id" `shouldMatch` uid
  profile <- Brig.getSelf user >>= getJSON 200
  profile %. "sso_id.scim_external_id" `shouldMatch` email
  login user email defPassword >>= assertStatus 403

  Brig.passwordReset user email >>= assertStatus 201
  reset <- BrigInternal.getPasswordResetCode user email >>= getJSON 200
  key <- reset %. "key" >>= asString
  code <- reset %. "code" >>= asString
  let newPassword = "new-synthetic-password"
  Brig.completePasswordReset user key code newPassword >>= assertSuccess
  login user email newPassword >>= assertStatus 200
  login user email defPassword >>= assertStatus 403

testLoginScimOnlyIdentityPreservesPasswordAndEmailChallenge :: (HasCallStack) => App ()
testLoginScimOnlyIdentityPreservesPasswordAndEmailChallenge = do
  (owner, team, []) <- createTeam OwnDomain 0
  token <- Spar.createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  scimUser <- randomScimUser
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  uid <- Spar.createScimUser owner token scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  registerInvitedUser OwnDomain team email
  user <- BrigInternal.getUsersId OwnDomain [uid] >>= getJSON 200 >>= asList >>= assertOne
  user %. "sso_id.scim_external_id" `shouldMatch` (scimUser %. "externalId")
  login user email defPassword >>= assertStatus 200
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled" >>= assertSuccess
  login user email defPassword `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-required"
  generateVerificationCode user email
  code <- getVerificationCode user "login" >>= getJSON 200 >>= asString
  loginWith2ndFactor user email defPassword code >>= assertStatus 200

testLoginSamlDoesNotRequireEmailVerificationCode :: (HasCallStack) => App ()
testLoginSamlDoesNotRequireEmailVerificationCode = do
  (owner, team, []) <- createTeam OwnDomain 0
  setTeamFeatureStatus owner team "mfa-totp" "disabled" >>= assertSuccess
  setTeamFeatureStatus owner team "sso" "enabled" >>= assertSuccess
  setTeamFeatureStatus owner team "validateSAMLemails" "enabled" >>= assertSuccess
  (idpResponse, idpCredentials) <- registerTestIdPWithMetaWithPrivateCreds owner
  idp <- idpResponse & getJSON 201 >>= (%. "id") >>= asString
  token <- Spar.createScimToken owner def {Spar.idp = Just idp} >>= getJSON 200 >>= (%. "token") >>= asString
  scimUser <- randomScimUser
  subject <- scimUser %. "externalId" >>= asString
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  uid <- Spar.createScimUser OwnDomain token scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  activateEmail OwnDomain email
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled" >>= assertSuccess
  -- The mock IdP signs a valid assertion; Wire must not add the email challenge.
  (loggedInUser, _) <- loginWithSamlEmail True team subject (idp, idpCredentials)
  loggedInUser `shouldMatch` Just uid
  login OwnDomain email defPassword >>= assertStatus 403
