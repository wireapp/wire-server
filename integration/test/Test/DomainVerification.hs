{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | See also: "Test.EnterpriseLogin"
module Test.DomainVerification where

import API.Brig
import API.BrigInternal
import API.Common
import API.GalleyInternal (setTeamFeatureLockStatus, setTeamFeatureStatus)
import Control.Concurrent (threadDelay)
import SetupHelpers
import Test.DNSMock
import Testlib.Prelude

mkDomainRedirectBackend :: String -> Value
mkDomainRedirectBackend url = object ["domain_redirect" .= "backend", "backend_url" .= url]

testDomainVerificationGetOwnershipToken :: (HasCallStack) => App ()
testDomainVerificationGetOwnershipToken = do
  domain <- randomDomain
  challenge <- setupChallenge OwnDomain domain

  bindResponse (verifyDomain OwnDomain domain challenge.challengeId challenge.challengeToken) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "domain-verification-failed"

  registerTechnitiumRecord challenge.technitiumToken domain ("wire-domain." <> domain) "TXT" challenge.dnsToken

  -- verify domain
  bindResponse (verifyDomain OwnDomain domain challenge.challengeId challenge.challengeToken) $ \resp -> do
    resp.status `shouldMatchInt` 200
    void $ resp.json %. "domain_ownership_token" & asString

  -- the challenge should be deleted after successful verification
  verifyDomain OwnDomain domain challenge.challengeId challenge.challengeToken >>= assertStatus 404

testDomainVerificationOnPremFlow :: (HasCallStack) => App ()
testDomainVerificationOnPremFlow = do
  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain
  let ownershipToken = setup.ownershipToken

  -- cannot set config for non-preauthorized domain
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just ownershipToken)
        (mkDomainRedirectBackend "https://wire.example.com")
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  -- preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- post config without ownership token (this is not allowed)
  updateDomainRedirect
    OwnDomain
    domain
    Nothing
    (mkDomainRedirectBackend "https://wire.example.com")
    >>= assertStatus 400

  -- [customer admin] post config (happy flow)
  updateDomainRedirect
    OwnDomain
    domain
    (Just ownershipToken)
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
  void $ setupOwnershipToken OwnDomain domain
  wrongSetup <- setupOwnershipToken OwnDomain =<< randomDomain
  let wrongToken = wrongSetup.ownershipToken

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] post config with wrong token
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just wrongToken)
        (mkDomainRedirectBackend "https://wire.example.com")
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 401
      resp.json %. "label" `shouldMatch` "domain-registration-update-auth-failure"

testDomainVerificationOnPremFlowNoRegistration :: (HasCallStack) => App ()
testDomainVerificationOnPremFlowNoRegistration = do
  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- [customer admin] post no-registration config
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup.ownershipToken)
    (object ["domain_redirect" .= "no-registration"])
    >>= assertStatus 200

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "no-registration"

testDomainVerificationRemoveFailure :: (HasCallStack) => App ()
testDomainVerificationRemoveFailure = do
  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain

  -- [backoffice] preauth
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "pre-authorized"

  -- [customer admin] try to remove entry
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just setup.ownershipToken)
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
    (Just setup.ownershipToken)
    (object ["domain_redirect" .= "no-registration"])
    >>= assertStatus 200
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup.ownershipToken)
    (object ["domain_redirect" .= "remove"])
    >>= assertStatus 200

  -- removing again should fail
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just setup.ownershipToken)
        (object ["domain_redirect" .= "remove"])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testDomainVerificationLockedState :: (HasCallStack) => App ()
testDomainVerificationLockedState = do
  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain

  -- [backoffice] lock the domain (public email provider)
  domainRegistrationLock OwnDomain domain >>= assertStatus 204

  -- domain redirect cannot be updated
  -- as locking overwrites any previous entry, the auth token will also be removed,
  -- and this will result in an auth failure
  bindResponse
    ( updateDomainRedirect
        OwnDomain
        domain
        (Just setup.ownershipToken)
        (object ["domain_redirect" .= "no-registration"])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 401
      resp.json %. "label" `shouldMatch` "domain-registration-update-auth-failure"

testUpdateTeamInvite :: (HasCallStack) => App ()
testUpdateTeamInvite = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2

  domain <- randomDomain
  setup <- setupOwnershipToken OwnDomain domain

  bindResponse (authorizeTeam owner domain setup.ownershipToken) $ \resp -> do
    resp.status `shouldMatchInt` 402
    resp.json %. "label" `shouldMatch` "domain-registration-update-payment-required"

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  bindResponse (authorizeTeam mem domain setup.ownershipToken) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  -- admin should not be able to set team-invite if the team hasn't been authorized
  bindResponse
    ( updateTeamInvite owner domain (object ["team_invite" .= "team", "team" .= tid])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200

  -- non-admin should not be able to set team-invite
  bindResponse
    ( updateTeamInvite mem domain (object ["team_invite" .= "team", "team" .= tid])
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

  -- setting team invite to the wrong team should fail
  fakeTeamId <- randomId
  bindResponse (updateTeamInvite owner domain (object ["team_invite" .= "team", "team" .= fakeTeamId])) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

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
  domain <- randomDomain
  (owner, tid, _m : _) <- createTeam OwnDomain 2
  setup <- setupOwnershipToken OwnDomain domain

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200

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
  domain <- randomDomain
  -- set domain-redirect to locked
  domainRegistrationLock OwnDomain domain >>= assertStatus 204

  setup <- setupOwnershipToken OwnDomain domain

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  -- can't authorize a team when the domain is locked
  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 403

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

  authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200

  -- setting team-invite to allowed should fail for on-prem domains
  bindResponse (updateTeamInvite owner domain (object ["team_invite" .= "allowed"])) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-forbidden-for-domain-registration-state"

testDisabledEnterpriseService :: (HasCallStack) => App ()
testDisabledEnterpriseService = do
  domain <- randomDomain

  bindResponse (getDomainVerificationChallenge OtherDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 503
    resp.json %. "label" `shouldMatch` "enterprise-service-not-enabled"

testOverwriteOwnershipToken :: (HasCallStack) => App ()
testOverwriteOwnershipToken = do
  domain <- randomDomain
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- get an ownership token
  setup1 <- setupOwnershipToken OwnDomain domain
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup1.ownershipToken)
    (mkDomainRedirectBackend "https://wire1.example.com")
    >>= assertStatus 200

  -- get a second ownership token
  setup2 <- setupOwnershipToken OwnDomain domain
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup2.ownershipToken)
    (object ["domain_redirect" .= "remove"])
    >>= assertStatus 200

  -- the first ownership token is not valid anymore
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup1.ownershipToken)
    (mkDomainRedirectBackend "https://wire1.example.com")
    >>= assertStatus 401

testChallengeTtl :: (HasCallStack) => App ()
testChallengeTtl = withModifiedBackend
  (def {brigCfg = (setField "optSettings.setChallengeTTL" (2 :: Int))})
  $ \domain -> do
    registrationDomain <- randomDomain
    challenge <- getDomainVerificationChallenge domain registrationDomain >>= getJSON 200
    challengeId <- challenge %. "id" & asString
    challengeToken <- challenge %. "token" & asString

    -- wait until the challenge ttl expires
    liftIO $ threadDelay 2_500_000
    bindResponse (verifyDomain domain registrationDomain challengeId challengeToken) $ \resp -> do
      resp.status `shouldMatchInt` 404

testGetAndDeleteRegisteredDomains :: (HasCallStack) => App ()
testGetAndDeleteRegisteredDomains = do
  (owner, tid, mem : _) <- createTeam OwnDomain 2

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  expectedDomains <- replicateM 5 do
    domain <- randomDomain
    setup <- setupOwnershipToken OwnDomain domain
    authorizeTeam owner domain setup.ownershipToken >>= assertStatus 200
    pure domain

  bindResponse (getRegisteredDomainsByTeam owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    actualDomains <- resp.json %. "registered_domains" & asList >>= traverse (asString . (%. "domain"))
    actualDomains `shouldMatchSet` expectedDomains

  getRegisteredDomainsByTeam mem tid >>= assertStatus 403
  (otherTeamOwner, _, _) <- createTeam OwnDomain 2
  getRegisteredDomainsByTeam otherTeamOwner tid >>= assertStatus 403

  nonExistingDomain <- randomDomain
  deleteRegisteredTeamDomain owner tid nonExistingDomain >>= assertStatus 404
  let firstDomain = head expectedDomains
  deleteRegisteredTeamDomain mem tid firstDomain >>= assertStatus 403
  deleteRegisteredTeamDomain otherTeamOwner tid firstDomain >>= assertStatus 403

  let checkDelete :: [String] -> App ()
      checkDelete [] =
        bindResponse (getRegisteredDomainsByTeam owner tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          actualDomains <- resp.json %. "registered_domains" & asList
          length actualDomains `shouldMatchInt` 0
      checkDelete (domainToDelete : remainingDomains) = do
        bindResponse (deleteRegisteredTeamDomain owner tid domainToDelete) $ \resp -> do
          resp.status `shouldMatchInt` 204
        bindResponse (getRegisteredDomainsByTeam owner tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          actualDomains <- resp.json %. "registered_domains" & asList >>= traverse (asString . (%. "domain"))
          actualDomains `shouldMatchSet` remainingDomains
        checkDelete remainingDomains

  checkDelete expectedDomains

testGetDomainRegistrationUserExists :: (HasCallStack) => App ()
testGetDomainRegistrationUserExists = do
  domain <- randomDomain
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- create a user with email on this domain
  void
    $ randomUser
      OwnDomain
      def
        { email = Just ("paolo@" <> domain)
        }

  setup <- setupOwnershipToken OwnDomain domain
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend "https://wire.example.com")
    >>= assertStatus 200

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("sven@" <> domain)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "backend"
    resp.json %. "backend_url" `shouldMatch` "https://wire.example.com"
    lookupField resp.json "due_to_existing_account" `shouldMatch` (Nothing :: Maybe String)

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" <> domain)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "none"
    lookupField resp.json "backend_url" `shouldMatch` (Nothing :: Maybe String)
    resp.json %. "due_to_existing_account" `shouldMatch` True
