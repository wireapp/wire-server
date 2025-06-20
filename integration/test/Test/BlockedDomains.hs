{-# LANGUAGE DuplicateRecordFields #-}

module Test.BlockedDomains where

import API.Brig as Brig
import API.Common
import Data.Yaml (array)
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

-- TODO: Register a user with normal domain email and change it do blocked domain email
