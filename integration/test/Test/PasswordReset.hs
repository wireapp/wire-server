module Test.PasswordReset where

import API.Brig
import API.BrigInternal
import API.Common
import SetupHelpers
import Testlib.Prelude

-- @SF.Provisioning @TSFI.RESTfulAPI @S1
--
-- This test checks the password reset functionality of the application.
-- Besides a successful password reset the following scenarios are tested:
-- - Subsequent password reset requests should succeed without errors.
-- - Attempting to reset the password with an incorrect key or code should fail.
-- - Attempting to log in with the old password after a successful reset should fail.
-- - Attempting to log in with the new password after a successful reset should succeed.
-- - Attempting to reset the password again to the same new password should fail.
testPasswordResetShouldSucceedButFailOnWrongInputs :: App ()
testPasswordResetShouldSucceedButFailOnWrongInputs = do
  u <- randomUser OwnDomain def
  email <- u %. "email" & asString
  passwordReset OwnDomain email >>= assertSuccess
  -- Even though a password reset is now in progress
  -- we expect a successful response from a subsequent request to not leak any information
  -- about the requested email.
  passwordReset OwnDomain email >>= assertSuccess

  (key, code) <- getPasswordResetData email
  let newPassword = "newpassword"

  -- complete password reset with incorrect key/code should fail
  completePasswordReset OwnDomain "wrong-key" code newPassword >>= assertStatus 400
  login OwnDomain email newPassword >>= assertStatus 403
  completePasswordReset OwnDomain key "wrong-code" newPassword >>= assertStatus 400
  login OwnDomain email newPassword >>= assertStatus 403

  -- complete password reset with correct key and code should succeed
  completePasswordReset OwnDomain key code newPassword >>= assertSuccess

  -- try login with old password should fail
  login OwnDomain email defPassword >>= assertStatus 403
  -- login with new password should succeed
  login OwnDomain email newPassword >>= assertSuccess
  -- reset password again to the same new password should fail
  passwordReset OwnDomain email >>= assertSuccess
  (nextKey, nextCode) <- getPasswordResetData email
  bindResponse (completePasswordReset OwnDomain nextKey nextCode newPassword) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "password-must-differ"

-- @END

getPasswordResetData :: String -> App (String, String)
getPasswordResetData email = do
  bindResponse (getPasswordResetCode OwnDomain email) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (,) <$> (resp.json %. "key" & asString) <*> (resp.json %. "code" & asString)
