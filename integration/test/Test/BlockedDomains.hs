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
      (cookie, token) <-
        login domain validUserEmail defPassword `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          token <- resp.json %. "access_token" & asString
          let cookie = fromJust $ getCookie "zuid" resp
          pure ("zuid=" <> cookie, token)

      username2 <- randomName
      bindResponse (updateEmail validUser (username2 <> "@" <> blockedDomain) cookie token) $ \resp -> do
        resp.status `shouldMatchInt` 451
        resp.json %. "label" `shouldMatch` "domain-blocked-for-registration"

      updateEmail validUser (username2 <> "@" <> validDomain) cookie token >>= assertSuccess

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
