-- | See also: "Test.DomainVerification"
module Test.EnterpriseLogin where

import API.BrigInternal
import API.Common
import Testlib.Prelude

testDomainRegistrationLock :: (HasCallStack) => App ()
testDomainRegistrationLock = do
  domain <- randomDomain
  -- it should not yet exist
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  -- add to deny-list
  assertStatus 204 =<< domainRegistrationLock OwnDomain domain
  -- idempotent
  assertStatus 204 =<< domainRegistrationLock OwnDomain domain
  -- it got created
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "locked"
    resp.json %. "team_invite" `shouldMatch` "allowed"
  -- remove from deny-list
  assertStatus 204 =<< domainRegistrationUnlock OwnDomain domain
  -- check that it got removed
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationLockPreviousValueOverwritten :: (HasCallStack) => App ()
testDomainRegistrationLockPreviousValueOverwritten = do
  domain <- randomDomain
  -- pre-authorize
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
  -- lock
  assertStatus 204 =<< domainRegistrationLock OwnDomain domain
  -- check that it got overwritten
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "locked"

testDomainRegistrationUnlockErrorIfNotLocked :: (HasCallStack) => App ()
testDomainRegistrationUnlockErrorIfNotLocked = do
  domain <- randomDomain
  -- pre-authorize
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
  -- attempt to unlock should fail
  bindResponse (domainRegistrationUnlock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testDomainRegistrationPreAuthorize :: (HasCallStack) => App ()
testDomainRegistrationPreAuthorize = do
  domain <- randomDomain
  -- it should not yet exist
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  -- pre-authorize
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  -- idempotent
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  -- it got created
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationPreAuthorizeFailsIfLocked :: (HasCallStack) => App ()
testDomainRegistrationPreAuthorizeFailsIfLocked = do
  domain <- randomDomain
  -- add to deny-list
  assertStatus 204 =<< domainRegistrationLock OwnDomain domain
  -- pre-authorize
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"
  -- check that it was not set to pre-authorized
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "locked"
  -- remove from deny-list
  assertStatus 204 =<< domainRegistrationUnlock OwnDomain domain
  -- now it should work
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  -- domain redirect should be pre-authorized
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationPreAuthorizeDoesNotAlterTeamInvite :: (HasCallStack) => App ()
testDomainRegistrationPreAuthorizeDoesNotAlterTeamInvite = do
  domain <- randomDomain
  -- it should not yet exist
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  let update =
        object
          [ "domain_redirect" .= "none",
            "team_invite" .= "team",
            "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  -- pre-authorize
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
    resp.json %. "team_invite" `shouldMatch` "allowed"
    lookupField resp.json "team" `shouldMatch` (Nothing :: Maybe Value)
    lookupField resp.json "backend_url" `shouldMatch` (Nothing :: Maybe Value)

testDomainRegistrationQueriesDoNotCreateEntry :: (HasCallStack) => App ()
testDomainRegistrationQueriesDoNotCreateEntry = do
  domain <- randomDomain
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  assertStatus 404 =<< domainRegistrationUnlock OwnDomain domain
  assertStatus 404 =<< domainRegistrationUnAuthorize OwnDomain domain
  assertStatus 404 =<< getDomainRegistration OwnDomain domain

testDomainRegistrationUpdate :: (HasCallStack) => App ()
testDomainRegistrationUpdate = do
  domain <- randomDomain
  -- it should not yet exist
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  updateDomain domain
    $ object
      [ "domain_redirect" .= "backend",
        "backend_url" .= "https://example.com",
        "team_invite" .= "not-allowed"
      ]
  updateDomain domain
    $ object
      [ "domain_redirect" .= "sso",
        "sso_code" .= "f82bad56-df61-49c0-bc9a-dc45c8ee1000",
        "team_invite" .= "not-allowed"
      ]
  updateDomain domain
    $ object
      [ "domain_redirect" .= "no-registration",
        "team_invite" .= "team",
        "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"
      ]
  where
    updateDomain :: (HasCallStack) => String -> Value -> App ()
    updateDomain domain update = do
      -- update
      assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
      -- idempotent
      assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
      -- it got created
      bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain" `shouldMatch` domain
        resp.json %. "domain_redirect" `shouldMatch` (update %. "domain_redirect")
        resp.json %. "team_invite" `shouldMatch` (update %. "team_invite")
        lookupField resp.json "backend_url" `shouldMatch` lookupField update "backend_url"
        lookupField resp.json "sso_code" `shouldMatch` lookupField update "sso_code"
        lookupField resp.json "team" `shouldMatch` lookupField update "team"

testDomainRegistrationUpdateInvalidCases :: (HasCallStack) => App ()
testDomainRegistrationUpdateInvalidCases = do
  domain <- randomDomain
  checkUpdateFails domain $ object ["domain_redirect" .= "locked", "team_invite" .= "not-allowed"]
  checkUpdateFails domain $ object ["domain_redirect" .= "locked", "team_invite" .= "team", "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"]
  checkUpdateFails domain $ object ["domain_redirect" .= "backend", "backend_url" .= "https://example.com", "team_invite" .= "team", "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"]
  checkUpdateFails domain $ object ["domain_redirect" .= "backend", "backend_url" .= "https://example.com", "team_invite" .= "allowed"]
  where
    checkUpdateFails :: (HasCallStack) => String -> Value -> App ()
    checkUpdateFails domain update = do
      bindResponse (updateDomainRegistration OwnDomain domain update) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testDomainRegistrationPreAuthorizedToUnAuthorize :: (HasCallStack) => App ()
testDomainRegistrationPreAuthorizedToUnAuthorize = do
  domain <- randomDomain
  let update =
        object
          [ "domain_redirect" .= "pre-authorized",
            "team_invite" .= "allowed"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  assertStatus 204 =<< domainRegistrationUnAuthorize OwnDomain domain
  assertStatus 204 =<< domainRegistrationUnAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationBackendToUnAuthorize :: (HasCallStack) => App ()
testDomainRegistrationBackendToUnAuthorize = do
  domain <- randomDomain
  let update =
        object
          [ "domain_redirect" .= "backend",
            "backend_url" .= "https://example.com",
            "team_invite" .= "not-allowed"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "backend"
    resp.json %. "backend_url" `shouldMatch` "https://example.com"
    resp.json %. "team_invite" `shouldMatch` "not-allowed"
  assertStatus 204 =<< domainRegistrationUnAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "allowed"
  assertStatus 204 =<< domainRegistrationUnAuthorize OwnDomain domain

testDomainRegistrationNoRegistrationToUnAuthorize :: (HasCallStack) => App ()
testDomainRegistrationNoRegistrationToUnAuthorize = do
  domain <- randomDomain
  let update =
        object
          [ "domain_redirect" .= "no-registration",
            "team_invite" .= "not-allowed"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  assertStatus 204 =<< domainRegistrationUnAuthorize OwnDomain domain
  assertStatus 204 =<< domainRegistrationUnAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationUnAuthorizeFailureWhenLocked :: (HasCallStack) => App ()
testDomainRegistrationUnAuthorizeFailureWhenLocked = do
  domain <- randomDomain
  let update =
        object
          [ "domain_redirect" .= "locked",
            "team_invite" .= "allowed"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  assertStatus 403 =<< domainRegistrationUnAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "locked"
    resp.json %. "team_invite" `shouldMatch` "allowed"

testDomainRegistrationUnAuthorizeFailureWhenSso :: (HasCallStack) => App ()
testDomainRegistrationUnAuthorizeFailureWhenSso = do
  domain <- randomDomain
  let update =
        object
          [ "domain_redirect" .= "sso",
            "sso_code" .= "f82bad56-df61-49c0-bc9a-dc45c8ee1000",
            "team_invite" .= "team",
            "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  assertStatus 403 =<< domainRegistrationUnAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "sso"
    resp.json %. "team_invite" `shouldMatch` "team"

testDomainRegistrationDelete :: (HasCallStack) => App ()
testDomainRegistrationDelete = do
  domain <- randomDomain
  let update =
        object
          [ "domain_redirect" .= "sso",
            "sso_code" .= "f82bad56-df61-49c0-bc9a-dc45c8ee1000",
            "team_invite" .= "team",
            "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"
          ]
  assertStatus 204 =<< updateDomainRegistration OwnDomain domain update
  assertStatus 204 =<< deleteDomainRegistration OwnDomain domain
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  assertStatus 204 =<< deleteDomainRegistration OwnDomain domain
