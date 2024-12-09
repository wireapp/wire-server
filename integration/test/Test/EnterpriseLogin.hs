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

testDomainRegistrationLockPreviousValueOverridden :: App ()
testDomainRegistrationLockPreviousValueOverridden = error "todo"

testDomainRegistrationUnlockErrorIfNotLocked :: App ()
testDomainRegistrationUnlockErrorIfNotLocked = error "todo"

testDomainRegistrationUnlockDoesNotCreateEntry :: App ()
testDomainRegistrationUnlockDoesNotCreateEntry = do
  domain <- randomDomain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (domainRegistrationUnlock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
