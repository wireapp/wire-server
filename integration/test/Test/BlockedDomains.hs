module Test.BlockedDomains where

import API.Brig as Brig
import API.Brig as BrigAddUser (AddUser (..))
import API.Common
import Data.Yaml (array)
import SetupHelpers
import Testlib.Prelude

testCannotSendActivationCodeToBlockedDomain :: (HasCallStack) => App ()
testCannotSendActivationCodeToBlockedDomain = do
  let blockedDomain = "blocked.example.com"
      validDomain = "valid.example.com"
  withModifiedBackend
    def
      { brigCfg =
          setField
            "optSettings.setCustomerExtensions.domainsBlockedForRegistration"
            (array [fromString blockedDomain])
      }
    $ \domain -> do
      username <- randomName
      let validEmail = username <> "@" <> validDomain
      addUser domain def {BrigAddUser.email = Just validEmail} `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 201

      let blockedEmail = username <> "@" <> blockedDomain
      bindResponse (activateSend domain blockedEmail Nothing) $ \resp -> do
        resp.status `shouldMatchInt` 451
        resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

      let otherValidEmail = username <> "-1@" <> validDomain
      activateSend domain otherValidEmail Nothing >>= assertSuccess

testCannotChangeOwnEmailWithBlockedDomain :: (HasCallStack) => App ()
testCannotChangeOwnEmailWithBlockedDomain = do
  let blockedDomain = "blocked.example.com"
      validDomain = "valid.example.com"
  withModifiedBackend
    def
      { brigCfg =
          setField
            "optSettings.setCustomerExtensions.domainsBlockedForRegistration"
            (array [fromString blockedDomain])
      }
    $ \domain -> do
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

      putSelfEmail validUser cookie token (username2 <> "@" <> validDomain) >>= assertSuccess

testCannotChangeTeamMemberEmailWithBlockedDomain :: (HasCallStack) => App ()
testCannotChangeTeamMemberEmailWithBlockedDomain = do
  let blockedDomain = "blocked.example.com"
      validDomain = "valid.example.com"
  withModifiedBackend
    def
      { brigCfg =
          setField
            "optSettings.setCustomerExtensions.domainsBlockedForRegistration"
            (array [fromString blockedDomain])
      }
    $ \domain -> do
      (owner, _team, [mem1]) <- createTeam domain 2

      username <- randomName
      bindResponse (putUserEmail owner mem1 (username <> "@" <> blockedDomain)) $ \resp -> do
        resp.status `shouldMatchInt` 451
        resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

      putUserEmail owner mem1 (username <> "@" <> validDomain) >>= assertSuccess

      ownerUsername <- randomName
      bindResponse (putUserEmail owner owner (ownerUsername <> "@" <> blockedDomain)) $ \resp -> do
        resp.status `shouldMatchInt` 451
        resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

      putUserEmail owner owner (ownerUsername <> "@" <> validDomain) >>= assertSuccess

testCannotCreateTeamInvitationWithBlockedDomain :: (HasCallStack) => App ()
testCannotCreateTeamInvitationWithBlockedDomain = do
  let blockedDomain = "blocked.example.com"
      validDomain = "valid.example.com"
  withModifiedBackend
    def
      { brigCfg =
          setField
            "optSettings.setCustomerExtensions.domainsBlockedForRegistration"
            (array [fromString blockedDomain])
      }
    $ \domain -> do
      (owner, _team, []) <- createTeam domain 1

      username <- randomName
      bindResponse (postInvitation owner (PostInvitation (Just (username <> "@" <> blockedDomain)) Nothing))
        $ \resp -> do
          resp.status `shouldMatchInt` 451
          resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

      void $ postInvitation owner (PostInvitation (Just (username <> "@" <> validDomain)) Nothing) >>= getJSON 201
