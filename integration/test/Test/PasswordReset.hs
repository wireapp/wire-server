module Test.PasswordReset where

import API.Brig
import API.BrigInternal hiding (activate)
import API.Common
import SetupHelpers
import Testlib.Prelude

-- @SF.Provisioning @TSFI.RESTfulAPI @S1
--
-- This test checks the password reset functionality of the application.
-- Besides a successful password reset the following scenarios are tested:
-- - Attempting to reset the password with an incorrect key or code should fail.
-- - Attempting to log in with the old password after a successful reset should fail.
-- - Attempting to log in with the new password after a successful reset should succeed.
-- - Attempting to reset the password again to the same new password should fail.
testPasswordResetShouldSucceedButFailOnWrongInputs :: (HasCallStack) => App ()
testPasswordResetShouldSucceedButFailOnWrongInputs = do
  u <- randomUser OwnDomain def
  email <- u %. "email" & asString
  passwordReset u email >>= assertSuccess

  (key, code) <- getPasswordResetData email
  let newPassword = "newpassword"

  -- complete password reset with incorrect key/code should fail
  completePasswordReset u "wrong-key" code newPassword >>= assertStatus 400
  login u email newPassword >>= assertStatus 403
  completePasswordReset u key "wrong-code" newPassword >>= assertStatus 400
  login u email newPassword >>= assertStatus 403

  -- complete password reset with correct key and code should succeed
  completePasswordReset u key code newPassword >>= assertSuccess

  -- try login with old password should fail
  login u email defPassword >>= assertStatus 403
  -- login with new password should succeed
  login u email newPassword >>= assertSuccess
  -- reset password again to the same new password should fail
  passwordReset u email >>= assertSuccess
  (nextKey, nextCode) <- getPasswordResetData email
  bindResponse (completePasswordReset u nextKey nextCode newPassword) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "password-must-differ"

-- @END

testPasswordResetAfterEmailUpdate :: (HasCallStack) => App ()
testPasswordResetAfterEmailUpdate = do
  u <- randomUser OwnDomain def
  email <- u %. "email" & asString
  (cookie, token) <- bindResponse (login u email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` 200
    token <- resp.json %. "access_token" & asString
    let cookie = fromJust $ getCookie "zuid" resp
    pure ("zuid=" <> cookie, token)

  -- initiate email update
  newEmail <- randomEmail
  updateEmail u newEmail cookie token >>= assertSuccess

  -- initiate password reset
  passwordReset u email >>= assertSuccess
  (key, code) <- getPasswordResetData email

  -- activate new email
  bindResponse (getActivationCode u newEmail) $ \resp -> do
    resp.status `shouldMatchInt` 200
    activationKey <- resp.json %. "key" & asString
    activationCode <- resp.json %. "code" & asString
    activate u activationKey activationCode >>= assertSuccess

  bindResponse (getSelf u) $ \resp -> do
    actualEmail <- resp.json %. "email"
    actualEmail `shouldMatch` newEmail

  -- attempting to complete password reset should fail
  bindResponse (completePasswordReset u key code "newpassword") $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-code"

testPasswordResetInvalidPasswordLength :: App ()
testPasswordResetInvalidPasswordLength = do
  u <- randomUser OwnDomain def
  email <- u %. "email" & asString
  passwordReset u email >>= assertSuccess
  (key, code) <- getPasswordResetData email

  -- complete password reset with a password that is too short should fail
  let shortPassword = "123456"
  completePasswordReset u key code shortPassword >>= assertStatus 400

  -- try login with new password should fail
  login u email shortPassword >>= assertStatus 403

getPasswordResetData :: String -> App (String, String)
getPasswordResetData email = do
  bindResponse (getPasswordResetCode OwnDomain email) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (,) <$> (resp.json %. "key" & asString) <*> (resp.json %. "code" & asString)
