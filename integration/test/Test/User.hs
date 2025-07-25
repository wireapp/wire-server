{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.User where

import API.Brig
import API.BrigInternal as I
import API.Common
import API.GalleyInternal
import qualified API.Spar as Spar
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool
import Testlib.VersionedFed

testSupportedProtocols :: (HasCallStack) => OneOf Domain (FedDomain 1) -> App ()
testSupportedProtocols bobDomain = do
  alice <- randomUser OwnDomain def
  alice %. "supported_protocols" `shouldMatchSet` ["proteus"]

  bob <- randomUser bobDomain def

  do
    -- bob sees default supported protocols for alice
    u <- getUser bob alice >>= getJSON 200
    u %. "supported_protocols" `shouldMatch` ["proteus"]

    p <- getUserSupportedProtocols bob alice >>= getJSON 200
    p `shouldMatch` ["proteus"]

  -- alice updates her supported protocols
  bindResponse (putUserSupportedProtocols alice ["proteus", "mls"]) $ \resp ->
    resp.status `shouldMatchInt` 200

  do
    -- bob sees the updated list
    u <- getUser bob alice >>= getJSON 200
    u %. "supported_protocols" `shouldMatchSet` ["proteus", "mls"]

    p <- getUserSupportedProtocols bob alice >>= getJSON 200
    p `shouldMatch` ["proteus", "mls"]

  -- invalid protocol name in update
  bindResponse (putUserSupportedProtocols alice ["proteus", "mls", "mixed"]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

testCreateUserSupportedProtocols :: (HasCallStack) => App ()
testCreateUserSupportedProtocols = do
  alice <- randomUser OwnDomain def {supportedProtocols = Just ["proteus", "mls"]}
  bindResponse (getUserSupportedProtocols alice alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` ["proteus", "mls"]

  bindResponse (createUser OwnDomain def {supportedProtocols = Just ["proteus", "mixed"]}) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

testRemoveMlsSupportShouldFail :: (HasCallStack) => App ()
testRemoveMlsSupportShouldFail = do
  alice <- randomUser OwnDomain def {supportedProtocols = Just ["proteus", "mls"]}
  bindResponse (getUserSupportedProtocols alice alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` ["proteus", "mls"]

  putUserSupportedProtocols alice ["proteus"] `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "mls-protocol-error"

  bindResponse (getUserSupportedProtocols alice alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` ["proteus", "mls"]

-- | For now this only tests attempts to update /self/handle in E2EId-enabled teams.  More
-- tests can be found under `/services/brig/test/integration` (and should be moved here).
testUpdateHandle :: (HasCallStack) => App ()
testUpdateHandle = do
  -- create team with one member, without scim, but with `mlsE2EId` enabled.
  (owner, team, [mem1]) <- createTeam OwnDomain 2
  mem1id <- asString $ mem1 %. "id"

  let featureName = "mlsE2EId"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< setTeamFeatureStatus owner team featureName "enabled"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

  -- all as expected here.  (see the second time we check this at the end of the test for an
  -- explanation why we care.)
  bindResponse (getSelf mem1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "managed_by" `shouldMatch` "wire"
  bindResponse (getUsersId owner [mem1id]) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mb <- (assertOne =<< asList resp.json) %. "managed_by"
    mb `shouldMatch` "wire"

  -- mem1 attempts to update handle for the first time => success
  --
  -- this is desired, because without SCIM users need to pick their own handles initially.
  -- moreover it is fine, because if `handle == NULL`, no mls E2Eid client certs can be
  -- created.
  handle <- UUID.toString <$> liftIO UUID.nextRandom
  bindResponse (putHandle mem1 handle) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (putHandle mem1 handle) $ \resp -> do
    -- idempotency
    resp.status `shouldMatchInt` 200

  -- mem1 attempts to update handle again => failure
  handle2 <- UUID.toString <$> liftIO UUID.nextRandom
  bindResponse (putHandle mem1 handle2) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"

  -- now self thinks it is managed by "scim", so clients can block change attempts to handle,
  -- display name without adding E2EId-specific logic.  this is just a hack, though: `GET
  -- /self` is the only place where this is happening, other end-points still report the truth
  -- that is still stored correctly in the DB.
  --
  -- details: https://wearezeta.atlassian.net/browse/WPB-6189.
  -- FUTUREWORK: figure out a better way for clients to detect E2EId (V6?)
  bindResponse (getSelf mem1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "managed_by" `shouldMatch` "scim"
  bindResponse (getUsersId owner [mem1id]) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mb <- (assertOne =<< asList resp.json) %. "managed_by"
    mb `shouldMatch` "wire"
  bindResponse (Spar.getScimTokens owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "tokens" `shouldMatch` ([] @String)

-- | For now this only tests attempts to update one's own display name, email address, or
-- language in E2EId-enabled teams (ie., everything except handle).  More tests can be found
-- under `/services/brig/test/integration` (and should be moved here).
testUpdateSelf :: (HasCallStack) => Tagged "mode" TestUpdateSelfMode -> App ()
testUpdateSelf (MkTagged mode) = do
  -- create team with one member, without scim, but with `mlsE2EId` enabled.
  (owner, team, [mem1]) <- createTeam OwnDomain 2

  let featureName = "mlsE2EId"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< setTeamFeatureStatus owner team featureName "enabled"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

  case mode of
    TestUpdateDisplayName -> do
      -- blocked unconditionally
      someDisplayName <- UUID.toString <$> liftIO UUID.nextRandom
      before <- getSelf mem1
      bindResponse (putSelf mem1 def {name = Just someDisplayName}) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "managed-by-scim"
      after <- getSelf mem1
      void $ (before.json %. "name") `shouldMatch` (after.json %. "name")
    TestUpdateEmailAddress -> do
      -- allowed unconditionally *for owner* (this is a bit off-topic: team members can't
      -- change their email addresses themselves under any conditions)
      someEmail <- (<> "@example.com") . UUID.toString <$> liftIO UUID.nextRandom
      bindResponse (putSelfEmail owner someEmail) $ \resp -> do
        resp.status `shouldMatchInt` 200
    TestUpdateLocale -> do
      -- scim maps "User.preferredLanguage" to brig's locale field.  allowed unconditionally.
      -- we try two languages to make sure it doesn't work because it's already the active
      -- locale.
      forM_ ["uk", "he"] $ \someLocale ->
        bindResponse (putSelfLocale mem1 someLocale) $ \resp -> do
          resp.status `shouldMatchInt` 200

data TestUpdateSelfMode
  = TestUpdateDisplayName
  | TestUpdateEmailAddress
  | TestUpdateLocale
  deriving (Eq, Show, Generic)

testActivateAccountWithPhoneV5 :: (HasCallStack) => App ()
testActivateAccountWithPhoneV5 = do
  let dom = OwnDomain
  let phone = "+4912345678"
  let reqBody = Aeson.object ["phone" .= phone]
  activateUserV5 dom reqBody `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

testMigratingPasswordHashingAlgorithm :: (HasCallStack) => App ()
testMigratingPasswordHashingAlgorithm = do
  let argon2idOpts =
        object
          [ "algorithm" .= "argon2id",
            "iterations" .= (1 :: Int),
            "memory" .= (128 :: Int),
            "parallelism" .= (1 :: Int)
          ]
      cfgArgon2id =
        def
          { brigCfg =
              setField "settings.setPasswordHashingOptions" argon2idOpts
                >=> removeField "optSettings.setSuspendInactiveUsers",
            galleyCfg = setField "settings.passwordHashingOptions" argon2idOpts
          }
      cfgScrypt =
        def
          { brigCfg =
              setField "settings.setPasswordHashingOptions.algorithm" "scrypt"
                >=> removeField "optSettings.setSuspendInactiveUsers",
            galleyCfg = setField "settings.passwordHashingOptions.algorithm" "scrypt"
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain
    email1 <- randomEmail
    password1 <- randomString 20

    email2 <- randomEmail
    password2 <- randomString 20

    runCodensity (startDynamicBackend testBackend cfgScrypt) $ \_ -> do
      void $ randomUser domain (def {email = Just email1, password = Just password1})
      login domain email1 password1 >>= assertSuccess

    runCodensity (startDynamicBackend testBackend cfgArgon2id) $ \_ -> do
      login domain email1 password1 >>= assertSuccess

      -- Create second user to ensure that we're testing migrating back. This is
      -- not really needed because the login above rehashes the password, but it
      -- makes the test clearer.
      void $ randomUser domain (def {email = Just email2, password = Just password2})
      login domain email2 password2 >>= assertSuccess

    -- Check that both users can still login with Scrypt in case the operator
    -- wants to rollback the config.
    runCodensity (startDynamicBackend testBackend cfgScrypt) $ \_ -> do
      login domain email1 password1 >>= assertSuccess
      login domain email2 password2 >>= assertSuccess

testUpdateEmailForEmailDomainForAnotherBackend :: (HasCallStack) => App ()
testUpdateEmailForEmailDomainForAnotherBackend = forM_ [ExplicitVersion 8, Versioned] \version -> do
  emailDomain <- randomDomain
  user <- randomUser OwnDomain def
  email <- user %. "email" & asString
  (cookie, token) <- bindResponse (login user email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` 200
    token <- resp.json %. "access_token" & asString
    let cookie = fromJust $ getCookie "zuid" resp
    pure ("zuid=" <> cookie, token)

  I.domainRegistrationPreAuthorize OwnDomain emailDomain >>= assertStatus 204
  setup <- setupOwnershipTokenForBackend OwnDomain emailDomain
  updateDomainRedirect
    OwnDomain
    version
    emailDomain
    (Just setup.ownershipToken)
    (mkDomainRedirectBackend version "https://example.com" "https://webapp.example.com")
    >>= assertStatus 200

  let newEmail = "galadriel@" <> emailDomain
  updateEmail user newEmail cookie token `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "condition-failed"

  bindResponse (getActivationCode user newEmail) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "not-found"

  bindResponse (getSelf user) $ \resp -> do
    resp.json %. "email" `shouldMatch` email

testActivateEmailForEmailDomainForAnotherBackend :: (HasCallStack) => App ()
testActivateEmailForEmailDomainForAnotherBackend = do
  tid <- randomId
  sso <- randomId
  object
    [ "domain_redirect" .= "backend",
      "backend"
        .= object
          [ "config_url" .= "https://example.com",
            "webapp_url" .= "https://webapp.example.com"
          ],
      "team_invite"
        .= "not-allowed"
    ]
    & testActivateEmailShouldBeAllowed False
  object
    [ "domain_redirect" .= "none",
      "team_invite" .= "allowed"
    ]
    & testActivateEmailShouldBeAllowed True
  object
    [ "domain_redirect" .= "no-registration",
      "team_invite" .= "team",
      "team" .= tid
    ]
    & testActivateEmailShouldBeAllowed False
  object
    [ "domain_redirect" .= "no-registration",
      "team_invite" .= "not-allowed"
    ]
    & testActivateEmailShouldBeAllowed False
  object
    [ "domain_redirect" .= "sso",
      "sso_code" .= sso,
      "team_invite" .= "not-allowed"
    ]
    & testActivateEmailShouldBeAllowed False
  object
    [ "domain_redirect" .= "sso",
      "sso_code" .= sso,
      "team_invite" .= "team",
      "team" .= tid
    ]
    & testActivateEmailShouldBeAllowed False
  where
    testActivateEmailShouldBeAllowed :: (HasCallStack) => Bool -> Value -> App ()
    testActivateEmailShouldBeAllowed activateAllowed update = do
      emailDomain <- randomDomain
      user <- randomUser OwnDomain def
      email <- user %. "email" & asString
      (cookie, token) <- bindResponse (login user email defPassword) $ \resp -> do
        resp.status `shouldMatchInt` 200
        token <- resp.json %. "access_token" & asString
        let cookie = fromJust $ getCookie "zuid" resp
        pure ("zuid=" <> cookie, token)

      let newEmail = "galadriel@" <> emailDomain
      updateEmail user newEmail cookie token >>= assertSuccess

      (key, code) <- bindResponse (getActivationCode user newEmail) $ \resp -> do
        resp.status `shouldMatchInt` 200
        (,)
          <$> (resp.json %. "key" & asString)
          <*> (resp.json %. "code" & asString)

      I.updateDomainRegistration OwnDomain emailDomain update >>= assertSuccess

      if activateAllowed
        then do
          API.Brig.activate user key code >>= assertSuccess
          getSelf user `bindResponse` \resp -> do
            resp.json %. "email" `shouldMatch` newEmail
        else do
          API.Brig.activate user key code `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 403
            resp.json %. "label" `shouldMatch` "condition-failed"

          getSelf user `bindResponse` \resp -> do
            resp.json %. "email" `shouldMatch` email

testPasswordChange :: (HasCallStack) => App ()
testPasswordChange =
  withModifiedBackend
    def
      { brigCfg =
          -- Disable password hashing rate limiting, so we can create enable services quickly
          setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
      }
    $ \domain -> do
      user <- randomUser domain def
      email <- asString $ user %. "email"
      newPassword <- randomString 20

      putPassword user defPassword defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "password-must-differ"

      putPassword user defPassword newPassword >>= assertSuccess

      login domain email defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "invalid-credentials"
      login domain email newPassword >>= assertSuccess

testEphemeralUserCreation :: (HasCallStack) => TaggedBool "ephemeral-user-creation-enabled" -> App ()
testEphemeralUserCreation (TaggedBool enabled) = do
  withModifiedBackend
    def
      { brigCfg = setField "optSettings.setEphemeralUserCreationEnabled" enabled
      }
    $ \domain -> do
      registerEphemeralUser domain `bindResponse` \resp -> do
        if enabled
          then do
            resp.status `shouldMatchInt` 201
          else do
            resp.status `shouldMatchInt` 403
            resp.json %. "label" `shouldMatch` "ephemeral-user-creation-disabled"

        registerUserWithEmail domain >>= assertSuccess
  where
    registerEphemeralUser domain = addUser domain def
    registerUserWithEmail domain = addUser domain def {email = Just ("user@" <> domain)}
