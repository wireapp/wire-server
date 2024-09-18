module Test.PasswordReset where

import API.Brig
import API.BrigInternal
import API.Common
import SetupHelpers
import Testlib.Prelude

testPasswordReset :: App ()
testPasswordReset = do
  u <- randomUser OwnDomain def
  email <- u %. "email" & asString
  passwordReset OwnDomain email >>= assertSuccess
  -- even though a password reset is now in progress
  -- we expect a successful response from a subsequent request to not leak any information
  -- about the requested email
  passwordReset OwnDomain email >>= assertSuccess

  (key, code) <- getPasswordResetData email
  let newPw = "newpassword"
  completePasswordReset OwnDomain key code newPw >>= assertSuccess
  -- try login
  login OwnDomain email defPassword >>= assertStatus 403
  login OwnDomain email newPw >>= assertSuccess
  -- reset password again to the same new password, get 409 "must be different"
  passwordReset OwnDomain email >>= assertSuccess
  (nextKey, nextCode) <- getPasswordResetData email
  bindResponse (completePasswordReset OwnDomain nextKey nextCode newPw) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "password-must-differ"

getPasswordResetData :: String -> App (String, String)
getPasswordResetData email = do
  bindResponse (getPasswordResetCode OwnDomain email) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (,) <$> (resp.json %. "key" & asString) <*> (resp.json %. "code" & asString)
