module Test.EnterpriseLogin where

import API.BrigInternal
import API.Common
import Testlib.Prelude

testDomainRegistrationLock :: App ()
testDomainRegistrationLock = do
  domain <- randomDomain
  -- it should not yet exist
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  -- add to deny-list
  bindResponse (domainRegistrationLock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- idempotent
  bindResponse (domainRegistrationLock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- it got created
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "locked"
    resp.json %. "team_invite" `shouldMatch` "allowed"
  -- remove from deny-list
  bindResponse (domainRegistrationUnlock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- check that it got removed
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationLockPreviousValueOverwritten :: App ()
testDomainRegistrationLockPreviousValueOverwritten = do
  domain <- randomDomain
  -- pre-authorize
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
  -- lock
  bindResponse (domainRegistrationLock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- check that it got overwritten
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "locked"

testDomainRegistrationUnlockErrorIfNotLocked :: App ()
testDomainRegistrationUnlockErrorIfNotLocked = do
  domain <- randomDomain
  -- pre-authorize
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
  -- attempt to unlock should fail
  bindResponse (domainRegistrationUnlock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "unlock-error"

testDomainRegistrationPreAuthorize :: App ()
testDomainRegistrationPreAuthorize = do
  domain <- randomDomain
  -- it should not yet exist
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  -- pre-authorize
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- idempotent
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- it got created
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationPreAuthorizeFailsIfLocked :: App ()
testDomainRegistrationPreAuthorizeFailsIfLocked = do
  domain <- randomDomain
  -- add to deny-list
  bindResponse (domainRegistrationLock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- pre-authorize
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-domain-redirect"
  -- check that it was not set to pre-authorized
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "locked"
  -- remove from deny-list
  bindResponse (domainRegistrationUnlock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- now it should work
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  -- domain redirect should be pre-authorized
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
    resp.json %. "team_invite" `shouldMatch` "allowed"

-- testDomainRegistrationPreAuthorizeDoesNotAlterTeamInvite :: App ()
-- testDomainRegistrationPreAuthorizeDoesNotAlterTeamInvite = error "todo"

testDomainRegistrationUnlockDoesNotCreateEntry :: App ()
testDomainRegistrationUnlockDoesNotCreateEntry = do
  domain <- randomDomain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (domainRegistrationUnlock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
