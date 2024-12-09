module Test.EnterpriseLogin where

import API.BrigInternal
import API.Common
import Testlib.Prelude

testDomainRegistrationLock :: App ()
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

testDomainRegistrationLockPreviousValueOverwritten :: App ()
testDomainRegistrationLockPreviousValueOverwritten = do
  domain <- randomDomain
  -- pre-authorize
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"
  -- lock
  assertStatus 204 =<< domainRegistrationLock OwnDomain domain
  -- check that it got overwritten
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "locked"

testDomainRegistrationUnlockErrorIfNotLocked :: App ()
testDomainRegistrationUnlockErrorIfNotLocked = do
  domain <- randomDomain
  -- pre-authorize
  assertStatus 204 =<< domainRegistrationPreAuthorize OwnDomain domain
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

testDomainRegistrationPreAuthorizeFailsIfLocked :: App ()
testDomainRegistrationPreAuthorizeFailsIfLocked = do
  domain <- randomDomain
  -- add to deny-list
  assertStatus 204 =<< domainRegistrationLock OwnDomain domain
  -- pre-authorize
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-domain-redirect"
  -- check that it was not set to pre-authorized
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
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

testDomainRegistrationPreAuthorizeDoesNotAlterTeamInvite :: App ()
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
    resp.json %. "team_invite" `shouldMatch` "team"
    resp.json %. "team" `shouldMatch` "3bc23f21-dc03-4922-9563-c3beedf895db"
    lookupField resp.json "backend_url" `shouldMatch` (Nothing :: Maybe Value)

testDomainRegistrationDoesNotCreateEntry :: App ()
testDomainRegistrationDoesNotCreateEntry = do
  domain <- randomDomain
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  assertStatus 404 =<< domainRegistrationUnlock OwnDomain domain
  assertStatus 404 =<< domainRegistrationUnAuthorize OwnDomain domain
  assertStatus 404 =<< getDomainRegistration OwnDomain domain

testDomainRegistrationUpdate :: App ()
testDomainRegistrationUpdate = do
  domain <- randomDomain
  -- it should not yet exist
  assertStatus 404 =<< getDomainRegistration OwnDomain domain
  -- TODO: check invariants for domain_redirect and team_invite combination
  updateDomain domain
    $ object
      [ "domain_redirect" .= "backend",
        "backend_url" .= "https://example.com",
        "team_invite" .= "not-allowed"
      ]
  updateDomain domain
    $ object
      [ "domain_redirect" .= "sso",
        "sso_idp_id" .= "f82bad56-df61-49c0-bc9a-dc45c8ee1000",
        "team_invite" .= "allowed"
      ]
  updateDomain domain
    $ object
      [ "domain_redirect" .= "no-registration",
        "team_invite" .= "team",
        "team" .= "3bc23f21-dc03-4922-9563-c3beedf895db"
      ]
  where
    updateDomain :: String -> Value -> App ()
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
        mUrl <- lookupField update "backend_url"
        case mUrl of
          Just url -> resp.json %. "backend_url" `shouldMatch` url
          Nothing -> lookupField resp.json "backend_url" `shouldMatch` (Nothing :: Maybe Value)
        mSsoId <- lookupField update "sso_idp_id"
        case mSsoId of
          Just ssoId -> resp.json %. "sso_idp_id" `shouldMatch` ssoId
          Nothing -> lookupField resp.json "sso_idp_id" `shouldMatch` (Nothing :: Maybe Value)
        mTid <- lookupField update "team"
        case mTid of
          Just tid -> resp.json %. "team" `shouldMatch` tid
          Nothing -> lookupField resp.json "team" `shouldMatch` (Nothing :: Maybe Value)

testDomainRegistrationUnAuthorize :: App ()
testDomainRegistrationUnAuthorize = do
  domain <- randomDomain
  _
