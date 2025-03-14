{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | See also: "Test.EnterpriseLogin"
module Test.DomainVerification where

import API.Brig
import API.BrigInternal
import API.Common
import API.GalleyInternal (setTeamFeatureLockStatus, setTeamFeatureStatus)
import API.Spar
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
  void $ randomUser OwnDomain def {email = Just ("paolo@" <> domain)}

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
  checkUpdateRedirectSuccessful
    domain
    ownershipToken
    (mkDomainRedirectBackend "https://wire.example.com")

  -- idempotence
  checkUpdateRedirectSuccessful
    domain
    ownershipToken
    (mkDomainRedirectBackend "https://wire.example.com")

  -- [customer admin] update the previously set backend url
  checkUpdateRedirectSuccessful
    domain
    ownershipToken
    (mkDomainRedirectBackend "https://wire2.example.com")

  -- [customer admin] update to no-registration
  checkUpdateRedirectSuccessful
    domain
    ownershipToken
    (object ["domain_redirect" .= "no-registration"])

  -- idempotence
  checkUpdateRedirectSuccessful
    domain
    ownershipToken
    (object ["domain_redirect" .= "no-registration"])

  -- [customer admin] transition from no-registration back to backend
  checkUpdateRedirectSuccessful
    domain
    ownershipToken
    (mkDomainRedirectBackend "https://wire.example.com")
  where
    checkUpdateRedirectSuccessful :: (HasCallStack) => String -> String -> Value -> App ()
    checkUpdateRedirectSuccessful domain token config = do
      updateDomainRedirect
        OwnDomain
        domain
        (Just token)
        config
        >>= assertStatus 200

      bindResponse (getDomainRegistrationFromEmail OwnDomain ("sven@" ++ domain)) \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain_redirect" `shouldMatch` (config %. "domain_redirect")
        lookupField resp.json "backend_url" `shouldMatch` (lookupField config "backend_url")

      bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" ++ domain)) \resp -> do
        resp.status `shouldMatchInt` 200
        isBackend <- config %. "domain_redirect" >>= asString <&> (== "backend")
        if isBackend
          then do
            resp.json %. "domain_redirect" `shouldMatch` "no-registration"
            resp.json %. "due_to_existing_account" `shouldMatch` True
          else do
            resp.json %. "domain_redirect" `shouldMatch` (config %. "domain_redirect")
            lookupField resp.json "due_to_existing_account" `shouldMatch` (Nothing :: Maybe Bool)

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
  updateDomainRedirect
    OwnDomain
    domain
    (Just setup.ownershipToken)
    (object ["domain_redirect" .= "remove"])
    >>= assertSuccess

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

testGetDomainRegistrationUserExistsBackend :: (HasCallStack) => App ()
testGetDomainRegistrationUserExistsBackend = do
  domain <- randomDomain
  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  -- create a user with email on this domain
  void $ randomUser OwnDomain def {email = Just ("paolo@" <> domain)}

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
    lookupField resp.json "due_to_existing_account" `shouldMatch` (Nothing :: Maybe Bool)

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("paolo@" <> domain)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "no-registration"
    lookupField resp.json "backend_url" `shouldMatch` (Nothing :: Maybe String)
    resp.json %. "due_to_existing_account" `shouldMatch` True

testGetDomainRegistrationUserExistsSso :: (HasCallStack) => App ()
testGetDomainRegistrationUserExistsSso = do
  emailDomain <- randomDomain
  (owner, tid, mem : _) <- createTeamWithEmailDomain OwnDomain emailDomain 2
  memMail <- mem %. "email" & asString
  let paoloMail = "paolo@" <> emailDomain
  void $ randomUser OwnDomain def {email = Just paoloMail}
  let svenMail = "sven@" <> emailDomain
  void $ randomUser OwnDomain def {email = Just svenMail, team = True}
  let newUserMail = "newUser@" <> emailDomain

  setup <- setupOwnershipToken OwnDomain emailDomain

  -- enable domain registration feature
  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  authorizeTeam owner emailDomain setup.ownershipToken >>= assertStatus 200

  -- [customer admin] register a new idp and use it to set a team-invite config
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"
  updateTeamInvite owner emailDomain (object ["team_invite" .= "allowed", "sso" .= idpId])
    >>= assertStatus 200

  -- newUserMail is not registered yet
  bindResponse (getDomainRegistrationFromEmail OwnDomain newUserMail) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "sso"
    resp.json %. "sso_code" `shouldMatch` idpId

  void $ loginWithSaml True tid newUserMail (idpId, idpMeta)

  -- now the account exists, and but as this is an SSO user they should be directed to the SSO flow
  bindResponse (getDomainRegistrationFromEmail OwnDomain newUserMail) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "sso"
    resp.json %. "sso_code" `shouldMatch` idpId

  -- these have normal password accounts, and some are not members of the team
  for_ [memMail, paoloMail, svenMail] \email -> do
    bindResponse (getDomainRegistrationFromEmail OwnDomain email) \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "domain_redirect" `shouldMatch` "no-registration"
      lookupField resp.json "sso_code" `shouldMatch` (Nothing :: Maybe String)
      lookupField resp.json "due_to_existing_account" `shouldMatch` (Nothing :: Maybe Bool)

testSsoLoginNoEmailVerification :: (HasCallStack) => App ()
testSsoLoginNoEmailVerification = do
  (owner, tid, _) <- createTeam OwnDomain 1
  emailDomain <- randomDomain

  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  setup <- setupOwnershipToken OwnDomain emailDomain
  authorizeTeam owner emailDomain setup.ownershipToken >>= assertSuccess

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"

  updateTeamInvite owner emailDomain (object ["team_invite" .= "not-allowed", "sso" .= idpId]) >>= assertSuccess

  let email = "user@" <> emailDomain
  (Just uid, _) <- loginWithSaml True tid email (idpId, idpMeta)
  getUsersId OwnDomain [uid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

  otherEmailDomain <- randomDomain
  let otherEmail = "otherUser@" <> otherEmailDomain
  (Just otherUid, _) <- loginWithSaml True tid otherEmail (idpId, idpMeta)

  getUsersId OwnDomain [otherUid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    lookupField user "email" `shouldMatch` (Nothing :: Maybe String)

  getUsersByEmail OwnDomain [otherEmail] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json >>= asList >>= shouldBeEmpty

  activateEmail OwnDomain otherEmail
  getUsersId OwnDomain [otherUid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` otherEmail

testScimOnlyWithRegisteredEmailDomain :: (HasCallStack) => App ()
testScimOnlyWithRegisteredEmailDomain = do
  (owner, tid, _) <- createTeam OwnDomain 1
  emailDomain <- randomDomain

  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  setup <- setupOwnershipToken OwnDomain emailDomain
  authorizeTeam owner emailDomain setup.ownershipToken >>= assertSuccess

  updateTeamInvite owner emailDomain (object ["team_invite" .= "team", "team" .= tid]) >>= assertSuccess

  tok <- createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  let email = "user@" <> emailDomain
      extId = email
  scimUser <- randomScimUserWith extId email
  uid <- bindResponse (createScimUser owner tok scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id" >>= asString
  registerInvitedUser OwnDomain tid email
  bindResponse (login OwnDomain email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` 200
  getUsersId OwnDomain [uid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email
  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

testScimAndSamlWithRegisteredEmailDomain :: (HasCallStack) => App ()
testScimAndSamlWithRegisteredEmailDomain = do
  (owner, tid, _) <- createTeam OwnDomain 1
  emailDomain <- randomDomain

  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  setup <- setupOwnershipToken OwnDomain emailDomain
  authorizeTeam owner emailDomain setup.ownershipToken >>= assertSuccess

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"

  updateTeamInvite owner emailDomain (object ["team_invite" .= "not-allowed", "sso" .= idpId]) >>= assertSuccess

  tok <-
    createScimToken owner def {idp = Just idpId}
      >>= getJSON 200
      >>= (%. "token")
      >>= asString
  let email = "user@" <> emailDomain
      extId = email
  scimUser <- randomScimUserWith extId email
  uid <- bindResponse (createScimUser owner tok scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id" >>= asString
  void $ loginWithSaml True tid email (idpId, idpMeta)

  getUsersId OwnDomain [uid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

testVerificationRequiredIfEmailDomainRedirectNotSso :: (HasCallStack) => App ()
testVerificationRequiredIfEmailDomainRedirectNotSso = do
  (owner, tid, _) <- createTeam OwnDomain 1
  emailDomain <- randomDomain

  assertSuccess =<< do
    setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
    setTeamFeatureStatus owner tid "domainRegistration" "enabled"

  setup <- setupOwnershipToken OwnDomain emailDomain
  authorizeTeam owner emailDomain setup.ownershipToken >>= assertSuccess

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"

  updateTeamInvite owner emailDomain (object ["team_invite" .= "team", "team" .= tid]) >>= assertSuccess

  let email = "user@" <> emailDomain
  (Just uid, _) <- loginWithSaml True tid email (idpId, idpMeta)

  getUsersId OwnDomain [uid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    lookupField user "email" `shouldMatch` (Nothing :: Maybe String)

  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json >>= asList >>= shouldBeEmpty

  activateEmail OwnDomain email
  getUsersId OwnDomain [uid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email
