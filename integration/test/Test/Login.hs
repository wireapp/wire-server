{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Login where

import API.BrigInternal (getVerificationCode)
import API.Common (defPassword)
import API.GalleyInternal
import API.Nginz (login, loginWith2ndFactor)
import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import SetupHelpers
import Testlib.Prelude

testLoginVerify6DigitEmailCodeSuccess :: HasCallStack => App ()
testLoginVerify6DigitEmailCodeSuccess = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  code <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
  bindResponse (loginWith2ndFactor owner email defPassword code) $ \resp -> do
    resp.status `shouldMatchInt` 200

testLoginVerificationCodeCanOnlyBeUsedOnce :: HasCallStack => App ()
testLoginVerificationCodeCanOnlyBeUsedOnce = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  code <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
  bindResponse (loginWith2ndFactor owner email defPassword code) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (loginWith2ndFactor owner email defPassword code) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that login fails with wrong second factor email verification code
testLoginVerify6DigitWrongCodeFails :: HasCallStack => App ()
testLoginVerify6DigitWrongCodeFails = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  bindResponse (loginWith2ndFactor owner email defPassword "123456") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that login without verification code fails if SndFactorPasswordChallenge feature is enabled in team
testLoginVerify6DigitMissingCodeFails :: HasCallStack => App ()
testLoginVerify6DigitMissingCodeFails = do
  (owner, team, []) <- createTeam OwnDomain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  bindResponse (login owner email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "code-authentication-required"

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that login fails with expired second factor email verification code
testLoginVerify6DigitExpiredCodeFails :: HasCallStack => App ()
testLoginVerify6DigitExpiredCodeFails = do
  withModifiedBackend
    (def {brigCfg = setField "optSettings.setVerificationTimeout" (Aeson.Number 2)})
    $ \domain -> do
      (owner, team, []) <- createTeam domain 0
      email <- owner %. "email"
      setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
      setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
      generateVerificationCode owner email
      code <- getVerificationCode owner "login" >>= getJSON 200 >>= asString
      liftIO $ threadDelay 2_000_100
      bindResponse (loginWith2ndFactor owner email defPassword code) \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- @END