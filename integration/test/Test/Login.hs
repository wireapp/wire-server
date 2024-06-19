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
