{-# LANGUAGE DuplicateRecordFields #-}

module Test.BlockedDomains where

import API.Brig as Brig
import API.Common
import Data.Yaml (array)
import SetupHelpers
import Testlib.Prelude

testCannotRegisterWithBlockedDomain :: (HasCallStack) => App ()
testCannotRegisterWithBlockedDomain = do
  let blockedDomain = "blocked.example.com"
  withModifiedBackend
    def
      { brigCfg =
          setField
            "optSettings.setCustomerExtensions.domainsBlockedForRegistration"
            ( array [fromString blockedDomain]
            )
      }
    $ \domain -> do
      username <- randomName
      let email = username <> "@" <> blockedDomain
      -- TODO: Wouldn't it be better to forbid registering with blocked domains?
      addUser domain def {email = Just email} `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 201

      bindResponse (activateSend domain email Nothing) $ \resp -> do
        resp.status `shouldMatchInt` 451
        resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

      validUser <- randomUser domain def
      validUserEmail <- validUser %. "email" & asString
      username2 <- randomName
      (cookie, token) <-
        login domain validUserEmail defPassword `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          token <- resp.json %. "access_token" & asString
          let cookie = fromJust $ getCookie "zuid" resp
          pure ("zuid=" <> cookie, token)

      bindResponse (putSelfEmail validUser cookie token (username2 <> "@" <> blockedDomain)) $ \resp -> do
        resp.status `shouldMatchInt` 451
        resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

-- TODO: Register a user with normal domain email and change it do blocked domain email
