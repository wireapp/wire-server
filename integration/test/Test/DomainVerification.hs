{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | See also: "Test.EnterpriseLogin"
module Test.DomainVerification where

import API.Brig
import API.BrigInternal
import API.Common
import API.GalleyInternal (setTeamFeatureLockStatus, setTeamFeatureStatus)
import SetupHelpers
import Test.DNSMock
import Testlib.Prelude

mkDomainRedirectBackend :: String -> Value
mkDomainRedirectBackend url = object ["domain_redirect" .= "backend", "backend_url" .= url]

testDomainVerificationOnPremFlow :: (HasCallStack) => App ()
testDomainVerificationOnPremFlow = do
  domain <- randomDomain

  -- [customer admin] fetch tokens
  (authToken, dnsToken) <-
    bindResponse (domainVerificationToken OwnDomain domain Nothing) $ \resp -> do
      resp.status `shouldMatchInt` 200
      authToken <- resp.json %. "auth_token" & asString
      dnsToken <- resp.json %. "dns_verification_token" & asString
      pure (authToken, dnsToken)

  -- cannot set config for non-preauthorized domain
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (mkDomainRedirectBackend "https://wire.example.com")
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] post config without auth token (this is not allowed)
  updateDomainRedirect
    OwnDomain
    domain
    Nothing
    (mkDomainRedirectBackend "https://wire.example.com")
    >>= assertStatus 400

  -- [customer admin] post config with auth token, but without creating the TXT DNS record
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (mkDomainRedirectBackend "https://wire.example.com")
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "domain-verification-failed"

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" "WRONG-DNS-TOKEN"

  -- [customer admin] post config with auth token, but with invalid TXT DNS record
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (mkDomainRedirectBackend "https://wire.example.com")
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "domain-verification-failed"

  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- [customer admin] post config (happy flow)
  updateDomainRedirect
    OwnDomain
    domain
    (Just authToken)
    (mkDomainRedirectBackend "https://wire.example.com")
    >>= assertStatus 200

  void $ randomUser OwnDomain def {email = Just ("paolo@" <> domain)}

  -- [customer user] pull the redirect config based on email
  bindResponse (getDomainRegistrationFromEmail OwnDomain ("sven@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "backend"
    resp.json %. "backend_url" `shouldMatch` "https://wire.example.com"

  -- [customer user] using a registered emails should return `none`
  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "none"

testDomainVerificationWrongAuth :: (HasCallStack) => App ()
testDomainVerificationWrongAuth = do
  domain <- randomDomain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] fetch tokens
  void $ domainVerificationToken OwnDomain domain Nothing >>= getJSON 200
  wrongToken <- generateWrongToken

  -- [customer admin] post config with wrong token
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just wrongToken)
        (mkDomainRedirectBackend "https://wire.example.com")
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "domain-verification-failed"
  where
    generateWrongToken :: App String
    generateWrongToken = do
      domain <- randomDomain
      tokens <- domainVerificationToken OwnDomain domain Nothing >>= getJSON 200
      tokens %. "auth_token" & asString

testDomainVerificationOnPremFlowNoRegistration :: (HasCallStack) => App ()
testDomainVerificationOnPremFlowNoRegistration = do
  domain <- randomDomain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] fetch tokens
  tokens <- domainVerificationToken OwnDomain domain Nothing >>= getJSON 200
  authToken <- tokens %. "auth_token" & asString
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    domain
    (Just authToken)
    (object ["domain_redirect" .= "no-registration"])
    >>= assertStatus 200

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "no-registration"

testDomainVerificationTokenIsVerifiedEverytime :: (HasCallStack) => App ()
testDomainVerificationTokenIsVerifiedEverytime = do
  domain <- randomDomain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] fetch tokens
  tokens <- domainVerificationToken OwnDomain domain Nothing >>= getJSON 200
  authToken <- tokens %. "auth_token" & asString
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    domain
    (Just authToken)
    (object ["domain_redirect" .= "no-registration"])
    >>= assertStatus 200

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "no-registration"

  deleteTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" "WRONG-TOKEN"

  -- [customer admin] post no-registration config - should fail
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (object ["domain_redirect" .= "remove"])
    )
    \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "domain-verification-failed"

testDomainVerificationRemoveFailure :: (HasCallStack) => App ()
testDomainVerificationRemoveFailure = do
  domain <- randomDomain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] fetch tokens
  tokens <- domainVerificationToken OwnDomain domain Nothing >>= getJSON 200
  authToken <- tokens %. "auth_token" & asString
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"

  -- [customer admin] try to remove entry
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (object ["domain_redirect" .= "remove"])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  -- check that it's still set to preauthorized
  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200

  -- [customer admin] set it to no-registration, then remove
  updateDomainRedirect
    OwnDomain
    domain
    (Just authToken)
    (object ["domain_redirect" .= "no-registration"])
    >>= assertStatus 200
  updateDomainRedirect
    OwnDomain
    domain
    (Just authToken)
    (object ["domain_redirect" .= "remove"])
    >>= assertStatus 200

  -- removing again should fail
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (object ["domain_redirect" .= "remove"])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testDomainVerificationLockedState :: (HasCallStack) => App ()
testDomainVerificationLockedState = do
  domain <- randomDomain

  -- [backoffice] lock the domain (public email provider)
  domainRegistrationLock OwnDomain domain >>= assertStatus 204

  -- tokens can still be fetched
  tokens <- domainVerificationToken OwnDomain domain Nothing >>= getJSON 200
  authToken <- tokens %. "auth_token" & asString
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- register DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- domain redirect cannot be updated
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just authToken)
        (object ["domain_redirect" .= "no-registration"])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testDomainVerificationLockedStateTeam :: (HasCallStack) => App ()
testDomainVerificationLockedStateTeam = do
  domain <- randomDomain
  (owner, tid, _m : _) <- createTeam OwnDomain 2

  -- set conference calling (paying team)
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "conferenceCalling" "unlocked"
    setTeamFeatureStatus owner tid "conferenceCalling" "enabled"

  -- [backoffice] lock the domain (public email provider)
  domainRegistrationLock OwnDomain domain >>= assertStatus 204

  -- tokens can still be fetched
  tokens <- domainVerificationTeamToken owner domain >>= getJSON 200
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- register DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- team invite cannot be updated
  bindResponse
    ( updateTeamInvite
        owner
        domain
        (object ["team_invite" .= "not-allowed"])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testUpdateTeamInvite :: (HasCallStack) => App ()
testUpdateTeamInvite = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2

  domain <- randomDomain

  bindResponse (domainVerificationTeamToken owner domain) $ \resp -> do
    resp.status `shouldMatchInt` 402
    resp.json %. "label" `shouldMatch` "domain-registration-updated-payment-required"

  -- set conference calling (paying team)
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "conferenceCalling" "unlocked"
    setTeamFeatureStatus owner tid "conferenceCalling" "enabled"

  bindResponse (domainVerificationTeamToken mem domain) $ \resp -> do
    resp.status `shouldMatchInt` 401
    resp.json %. "label" `shouldMatch` "domain-registration-updated-auth-failure"

  -- [customer admin] fetch tokens
  tokens <- domainVerificationTeamToken owner domain >>= getJSON 200
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- setting team invite to the wrong team should fail
  fakeTeamId <- randomId
  updateTeamInvite owner domain (object ["team_invite" .= "team", "team" .= fakeTeamId])
    >>= assertStatus 401

  -- non-admin should not be able to set team-invite
  bindResponse
    ( updateTeamInvite mem domain (object ["team_invite" .= "team", "team" .= tid])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 401
      resp.json %. "label" `shouldMatch` "domain-registration-updated-auth-failure"

  -- [customer admin] set team-invite to team
  updateTeamInvite owner domain (object ["team_invite" .= "team", "team" .= tid])
    >>= assertStatus 200

  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "team"
    resp.json %. "team" `shouldMatch` tid

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "none"

  -- [customer admin] set team-invite to not-allowed
  updateTeamInvite owner domain (object ["team_invite" .= "not-allowed"])
    >>= assertStatus 200

  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "none"
    resp.json %. "team_invite" `shouldMatch` "not-allowed"

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "none"

testUpdateTeamInviteSSO :: (HasCallStack) => App ()
testUpdateTeamInviteSSO = do
  (owner, tid, _m : _) <- createTeam OwnDomain 2

  -- set conference calling (paying team)
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "conferenceCalling" "unlocked"
    setTeamFeatureStatus owner tid "conferenceCalling" "enabled"

  domain <- randomDomain

  -- [customer admin] fetch tokens
  tokens <- domainVerificationTeamToken owner domain >>= getJSON 200
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- [customer admin] post team-invite config with an invalid idp
  fakeIdP <- randomId
  updateTeamInvite owner domain (object ["team_invite" .= "allowed", "sso" .= fakeIdP])
    >>= assertStatus 403

  -- [customer admin] register a new idp and use it to set a team-invite config
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  idp <- bindResponse (registerTestIdPWithMeta owner) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id"
  updateTeamInvite owner domain (object ["team_invite" .= "allowed", "sso" .= idp])
    >>= assertStatus 200

  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "sso"
    resp.json %. "team_invite" `shouldMatch` "allowed"
    resp.json %. "sso_code" `shouldMatch` idp

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "sso"
    resp.json %. "sso_code" `shouldMatch` idp

testUpdateTeamInviteLocked :: (HasCallStack) => App ()
testUpdateTeamInviteLocked = do
  (owner, tid, _m : _) <- createTeam OwnDomain 2

  -- set conference calling (paying team)
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "conferenceCalling" "unlocked"
    setTeamFeatureStatus owner tid "conferenceCalling" "enabled"

  domain <- randomDomain

  -- [customer admin] fetch tokens
  tokens <- domainVerificationTeamToken owner domain >>= getJSON 200
  dnsToken <- tokens %. "dns_verification_token" & asString

  -- [customer admin] register TXT DNS record
  tok <- getTechnitiumApiKey
  registerTechnitiumZone tok domain
  registerTechnitiumRecord tok domain ("wire-domain." <> domain) "TXT" dnsToken

  -- set domain-redirect to locked
  domainRegistrationLock OwnDomain domain >>= assertStatus 204

  -- setting team-invite to not-allowed should fail for locked domains
  bindResponse (updateTeamInvite owner domain (object ["team_invite" .= "not-allowed"])) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  updateDomainRegistration
    OwnDomain
    domain
    ( object
        [ "domain_redirect" .= "backend",
          "team_invite" .= "not-allowed",
          "backend_url" .= "https://wire.example.com"
        ]
    )
    >>= assertStatus 204

  -- setting team-invite to allowed should fail for on-prem domains
  bindResponse (updateTeamInvite owner domain (object ["team_invite" .= "allowed"])) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"
