{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-ambiguous-fields #-}

module Test.Spar where

import API.Brig as Brig
import API.BrigInternal as BrigInternal
import API.Common (defPassword, randomDomain, randomEmail, randomExternalId, randomHandle)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar
import API.SparInternal
import Control.Concurrent (threadDelay)
import Control.Lens (to, (^.))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions (cs)
import qualified Data.Text as ST
import Debug.Trace
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import qualified SAML2.WebSSO.Test.Util as SAML
import qualified SAML2.WebSSO.XML as SAMLXML
import SetupHelpers
import Testlib.JSON
import Testlib.PTest
import Testlib.Prelude

----------------------------------------------------------------------
-- scim stuff

testSparUserCreationInvitationTimeout :: (HasCallStack) => App ()
testSparUserCreationInvitationTimeout = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

  -- Trying to create the same user again right away should fail
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 409

  -- However, if we wait until the invitation timeout has passed
  -- It's currently configured to 1s local/CI.
  liftIO $ threadDelay (2_000_000)

  -- ...we should be able to create the user again
  retryT $ bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

testSparExternalIdDifferentFromEmailWithIdp :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmailWithIdp = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  void $ registerTestIdPWithMeta owner >>= getJSON 201
  tok <- createScimTokenV6 owner def >>= getJSON 200 >>= (%. "token") >>= asString
  email <- randomEmail
  extId <- randomExternalId
  scimUser <- randomScimUserWithEmail extId email
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  activateEmail OwnDomain email
  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json >>= asList >>= assertOne
    u %. "email" `shouldMatch` email
    subject <- u %. "sso_id.subject" >>= asString
    subject `shouldContainString` extId
    u %. "handle" `shouldMatch` (scimUser %. "userName")

  -- Verify that updating `userName` (handle) works
  scimUserWith1Update <- do
    newHandle <- randomHandle
    updatedScimUser <- setField "userName" newHandle scimUser
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "userName" `shouldMatch` newHandle
    checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
      u %. "externalId" `shouldMatch` extId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "handle" `shouldMatch` newHandle
    pure updatedScimUser

  -- Verify that updating the user's external ID works
  scimUserWith2Updates <- do
    newExtId <- randomExternalId
    updatedScimUser <- setField "externalId" newExtId scimUserWith1Update
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "externalId" `shouldMatch` newExtId
    checkSparGetUserAndFindByExtId OwnDomain tok newExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` newExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` email
      subject <- u %. "sso_id.subject" >>= asString
      subject `shouldContainString` newExtId
    bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
      res.json %. "totalResults" `shouldMatchInt` 0
      res.json %. "Resources" `shouldMatch` ([] :: [Value])
    pure updatedScimUser

  -- Verify that updating the user's email works
  do
    let oldEmail = email
    newEmail <- randomEmail
    updatedScimUser <- setField "emails" (toJSON [object ["value" .= newEmail]]) scimUserWith2Updates
    currentExtId <- updatedScimUser %. "externalId" >>= asString
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200

    -- before activation the old email should still be present
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` oldEmail
      subject <- u %. "sso_id.subject" >>= asString
      subject `shouldContainString` currentExtId

    -- after activation the new email should be present
    activateEmail OwnDomain newEmail
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` newEmail
      subject <- u %. "sso_id.subject" >>= asString
      subject `shouldContainString` currentExtId

testSparExternalIdDifferentFromEmail :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  email <- randomEmail
  extId <- randomExternalId
  scimUser <- randomScimUserWithEmail extId email
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json >>= asList >>= shouldBeEmpty

  registerInvitedUser OwnDomain tid email

  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json >>= asList >>= assertOne
    u %. "email" `shouldMatch` email
    u %. "sso_id.scim_external_id" `shouldMatch` extId
    u %. "handle" `shouldMatch` (scimUser %. "userName")

  -- Verify that updating the scim user works
  scimUserWith1Update <- do
    -- FUTUREWORK: test updating other fields besides handle as well
    newHandle <- randomHandle
    updatedScimUser <- setField "userName" newHandle scimUser
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "userName" `shouldMatch` newHandle
    checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
      u %. "externalId" `shouldMatch` extId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "handle" `shouldMatch` newHandle
    pure updatedScimUser

  -- Verify that updating the user's external ID works
  scimUserWith2Updates <- do
    newExtId <- randomExternalId
    updatedScimUser <- setField "externalId" newExtId scimUserWith1Update
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "externalId" `shouldMatch` newExtId
    checkSparGetUserAndFindByExtId OwnDomain tok newExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` newExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` email
      u %. "sso_id.scim_external_id" `shouldMatch` newExtId
    bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
      res.json %. "totalResults" `shouldMatchInt` 0
      res.json %. "Resources" `shouldMatch` ([] :: [Value])
    pure updatedScimUser

  -- Verify that updating the user's email works
  do
    let oldEmail = email
    newEmail <- randomEmail
    updatedScimUser <- setField "emails" (toJSON [object ["value" .= newEmail]]) scimUserWith2Updates
    currentExtId <- updatedScimUser %. "externalId" >>= asString
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200

    -- before activation the new email should be returned by the SCIM API
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    -- however brig should still return the old email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` oldEmail
      u %. "sso_id.scim_external_id" `shouldMatch` currentExtId

    -- after activation the new email should be present
    activateEmail OwnDomain newEmail
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` newEmail
      u %. "sso_id.scim_external_id" `shouldMatch` currentExtId

testSparExternalIdUpdateToANonEmail :: (HasCallStack) => App ()
testSparExternalIdUpdateToANonEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser >>= removeField "emails"
  email <- scimUser %. "externalId" >>= asString
  userId <- bindResponse (createScimUser OwnDomain tok scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    (resp.json %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString) `shouldMatch` email
    resp.json %. "id" >>= asString
  registerInvitedUser OwnDomain tid email

  let extId = "notanemailaddress"
  updatedScimUser <- setField "externalId" extId scimUser
  updateScimUser OwnDomain tok userId updatedScimUser >>= assertStatus 400

testSparMigrateFromExternalIdOnlyToEmail :: (HasCallStack) => Tagged "mailUnchanged" Bool -> App ()
testSparMigrateFromExternalIdOnlyToEmail (MkTagged emailUnchanged) = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser >>= removeField "emails"
  email <- scimUser %. "externalId" >>= asString
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  registerInvitedUser OwnDomain tid email

  -- Verify that updating a user with an empty emails does not change the email
  bindResponse (updateScimUser OwnDomain tok userId scimUser) $ \resp -> do
    resp.json %. "emails" `shouldMatch` (toJSON [object ["value" .= email]])
    resp.status `shouldMatchInt` 200

  newEmail <- if emailUnchanged then pure email else randomEmail
  let newEmails = (toJSON [object ["value" .= newEmail]])
  updatedScimUser <- setField "emails" newEmails scimUser
  updateScimUser OwnDomain tok userId updatedScimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "externalId" `shouldMatch` (updatedScimUser %. "externalId")
    resp.json %. "emails" `shouldMatch` (updatedScimUser %. "emails")

  -- after activation the new email should be present
  unless emailUnchanged $ activateEmail OwnDomain newEmail

  extId <- scimUser %. "externalId" >>= asString
  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json >>= asList >>= assertOne
    u %. "email" `shouldMatch` newEmail
    u %. "sso_id.scim_external_id" `shouldMatch` extId

checkSparGetUserAndFindByExtId :: (HasCallStack, MakesValue domain) => domain -> String -> String -> String -> (Value -> App ()) -> App ()
checkSparGetUserAndFindByExtId domain tok extId uid k = do
  usersByExtIdResp <- findUsersByExternalId domain tok extId
  usersByExtIdResp.status `shouldMatchInt` 200
  userByIdExtId <- usersByExtIdResp.json %. "Resources" >>= asList >>= assertOne
  k userByIdExtId

  userByUidResp <- getScimUser domain tok uid
  userByUidResp.status `shouldMatchInt` 200
  userByUid <- userByUidResp.json
  k userByUid

  userByUid `shouldMatch` userByIdExtId

testSparScimTokenLimit :: (HasCallStack) => App ()
testSparScimTokenLimit = withModifiedBackend
  def
    { brigCfg =
        -- Disable password hashing rate limiting, so we can create enable services quickly
        setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
    }
  $ \domain -> do
    (owner, _tid, _) <- createTeam domain 1
    replicateM_ 8 $ createScimToken owner def >>= assertSuccess
    createScimToken owner def `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "token-limit-reached"

testSparCreateScimTokenNoName :: (HasCallStack) => App ()
testSparCreateScimTokenNoName = do
  (owner, _tid, mem : _) <- createTeam OwnDomain 2
  createScimTokenV6 owner def >>= assertSuccess
  createScimTokenV6 owner def >>= assertSuccess
  tokens <- bindResponse (getScimTokens owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    tokens <- resp.json %. "tokens" >>= asList
    for_ tokens $ \token -> do
      token %. "name" `shouldMatch` (token %. "id")
    pure tokens
  for_ tokens $ \token -> do
    tokenId <- token %. "id" >>= asString
    putScimTokenName mem tokenId "new name" >>= assertStatus 403
    putScimTokenName owner tokenId ("token:" <> tokenId) >>= assertSuccess
  bindResponse (getScimTokens owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    updatedTokens <- resp.json %. "tokens" >>= asList
    for_ updatedTokens $ \token -> do
      tokenId <- token %. "id" >>= asString
      token %. "name" `shouldMatch` ("token:" <> tokenId)

-- | in V6, create idp then scim without idp id and idp id is unique
testSparCreateScimTokenAssocImplicitly :: (HasCallStack) => App ()
testSparCreateScimTokenAssocImplicitly = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  samlIdpId <- bindResponse (registerTestIdPWithMeta owner) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json %. "id"
  bindResponse (createScimTokenV6 owner def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "info.idp" `shouldMatch` samlIdpId
  bindResponse (getAllIdPs owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    idp <- resp.json %. "providers" >>= asList >>= assertOne
    idp %. "id" `shouldMatch` samlIdpId

-- | in V6, name should be ignored
testSparCreateScimTokenWithName :: (HasCallStack) => App ()
testSparCreateScimTokenWithName = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  let notExpected = "my scim token"
  createScimTokenV6 owner (def {name = Just notExpected}) >>= assertSuccess
  token <- getScimTokens owner >>= getJSON 200 >>= (%. "tokens") >>= asList >>= assertOne
  assoc <- token %. "id"
  token %. "name" `shouldMatch` Just assoc

----------------------------------------------------------------------
-- scim group stuff

testSparScimCreateUserGroup :: (HasCallStack) => App ()
testSparScimCreateUserGroup = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString

  -- f0, f1, ... are stolen from other tests in this module, but they don't wokr quite yet.

  {-
  let _f0 :: App String
      _f0 = do
        void $ setTeamFeatureStatus owner tid "sso" "enabled"
        void $ registerTestIdPWithMeta owner >>= getJSON 201
        email <- randomEmail
        extId <- randomExternalId
        scimUser <- randomScimUserWithEmail extId email
        scimUserId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
        bindResponse (getUsersId OwnDomain [scimUserId]) $ \res -> do
          res.status `shouldMatchInt` 200
          asString (res.json %. "[0].id") `shouldMatch` [scimUserId]
        pure scimUserId

   -}

  let f1 :: App String
      f1 = do
        assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" "disabled"
        assertSuccess =<< setTeamFeatureStatus owner tid "sso" "enabled"
        void $ registerTestIdPWithMetaWithPrivateCreds owner

        scimUser <-
          randomScimUserWith
            def
              { mkExternalId = randomEmail,
                prependExternalIdToEmails = False,
                mkOtherEmails = pure []
              }
        uid <- createScimUser owner tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

        getScimUser OwnDomain tok uid `bindResponse` \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "id" `shouldMatch` uid
          traceM (show owner)
          traceM (show tid)
          traceM . show =<< res.json -- if this looks right (team,
          -- id), then maybe there is another bug in scim group
          -- creation, not the test?
          pure uid

  scimUserId :: String <- f1
  let scimUserGroup =
        object
          [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            "displayName" .= "ze groop",
            "members"
              .= [ object
                     [ "type" .= "User",
                       "$ref" .= "...", -- something like
                       -- "https://example.org/v2/scim/User/ea2e4bf0-aa5e-11f0-96ad-e776a606779b"?
                       -- but since we're just receiving this it's ok
                       -- to ignore.
                       "value" .= scimUserId
                     ]
                 ]
          ]
  createScimUserGroup OwnDomain tok scimUserGroup >>= assertSuccess

----------------------------------------------------------------------
-- saml stuff

-- | In this test, the IdP attempts an IdP-initiated login, and the client gets redirected
-- back to IdP from SP with a valid authentication request.  This is to make some hypothetical
-- attacks harder while still supporting login dashboards in IdP UIs.
testSparEmulateSPInitiatedLogin :: (HasCallStack) => App ()
testSparEmulateSPInitiatedLogin = do
  -- set up saml-not-scim team
  (owner, tid, []) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (createIdpResp, (_idpmeta, privcreds)) <- registerTestIdPWithMetaWithPrivateCreds owner
  assertSuccess createIdpResp

  -- craft authnresp without req
  idpValue :: A.Value <- createIdpResp.json
  let idp :: SAML.IdPConfig Value
      idp = either error id $ A.parseEither (A.parseJSON @(SAML.IdPConfig A.Value)) idpValue
  authnresp <- getAuthnResponse tid idp privcreds

  -- send to finalize and check redirect response
  finalizeSamlLogin OwnDomain tid authnresp `bindResponse` \resp -> do
    -- the 303 is followed immediately, so the response is already coming from
    -- /sso/initiate-login here.
    resp.status `shouldMatchInt` 200
    (cs resp.body) `shouldContain` "SAMLRequest"

-- | UTF-8 chars (non-Latin-1) caused issues in XML parsing.
testSparSPInitiatedLoginWithUtf8 :: (HasCallStack) => App ()
testSparSPInitiatedLoginWithUtf8 = do
  -- set up saml-not-scim team
  (owner, tid, []) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (createIdpResp, (idpMeta, privcreds)) <- registerTestIdPWithMetaWithPrivateCreds owner
  assertSuccess createIdpResp

  -- gather info about idp and account
  idpValue :: A.Value <- createIdpResp.json
  randomness <- randomId
  let idp :: SAML.IdPConfig (Value {- not needed -})
      idp = either error id $ A.parseEither (A.parseJSON @(SAML.IdPConfig A.Value)) idpValue

      userName = "klăusﭲﭳﭴﭵﭶﭷﭸﭹﭺﭻﭼﭽﭾﭿㄖㄗㄘ✈✉♊ႩႪงจฉชซὨὩἈἉἊἋἌἍἎἏຜຝڈډڊڋ" ++ randomness
      Right (subject :: SAML.NameID) =
        SAML.mkNameID
          ((SAML.mkUNameIDUnspecified . ST.pack) userName)
          Nothing
          Nothing
          Nothing

  idpIdString <- asString $ idp ^. SAML.idpId

  -- login
  (Just uidString, _) <- loginWithSaml True tid subject (idpIdString, (idpMeta, privcreds))
  ownDomain <- objDomain OwnDomain
  Brig.getSelf' ownDomain uidString `bindResponse` \resp -> do
    resp.json %. "name" `shouldMatch` userName

-- | in V6, create two idps then one scim should fail
testSparCreateTwoScimTokensForOneIdp :: (HasCallStack) => App ()
testSparCreateTwoScimTokensForOneIdp = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  registerTestIdPWithMeta owner >>= assertSuccess
  registerTestIdPWithMeta owner >>= assertSuccess
  createScimTokenV6 owner def >>= assertStatus 400
  tokens <- getScimTokens owner >>= getJSON 200 >>= (%. "tokens") >>= asList
  length tokens `shouldMatchInt` 0

testCheckAdminGetTeamId :: (HasCallStack) => App ()
testCheckAdminGetTeamId = do
  (owner :: Value, tid :: String, [regular] :: [Value]) <- createTeam OwnDomain 2
  void $ setTeamFeatureStatus owner tid "sso" "enabled" -- required for the next request
  SAML.SampleIdP idpMeta _ _ _ <- SAML.makeSampleIdPMetadata
  createIdp owner idpMeta >>= assertSuccess -- Successful API response for owner (admin),
  createIdp regular idpMeta `bindResponse` \resp -> do
    -- insuficient permissions for non-admin, both as expected.
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "insufficient-permissions"

testCheckAdminGetTeamIdV7 :: App ()
testCheckAdminGetTeamIdV7 = withAPIVersion 7 testCheckAdminGetTeamId

testSsoLoginAndEmailVerification :: (HasCallStack) => App ()
testSsoLoginAndEmailVerification = do
  (owner, tid, _) <- createTeam OwnDomain 1
  emailDomain <- randomDomain

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"

  let email = "user@" <> emailDomain
  void $ loginWithSamlEmail True tid email (idpId, idpMeta)
  activateEmail OwnDomain email
  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

-- | This test may be covered by `testScimUpdateEmailAddress` and maybe can be removed.
testSsoLoginNoSamlEmailValidation :: (HasCallStack) => TaggedBool "validateSAMLEmails" -> App ()
testSsoLoginNoSamlEmailValidation (TaggedBool validateSAMLEmails) = do
  (owner, tid, _) <- createTeam OwnDomain 1
  emailDomain <- randomDomain

  let status = if validateSAMLEmails then "enabled" else "disabled"
  assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" status

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"

  let email = "user@" <> emailDomain
  (Just uid, authnResp) <- loginWithSamlEmail True tid email (idpId, idpMeta)
  let parsed :: SAML.AuthnResponse =
        fromRight (error "invalid authnResponse")
          . SAMLXML.parseFromDocument
          . SAML.fromSignedAuthnResponse
          $ authnResp
      uref = either (error . show) id $ SAML.assertionsToUserRef (parsed ^. SAML.rspPayload)
      eid = CI.original $ uref ^. SAML.uidSubject . to SAML.unsafeShowNameID
  eid `shouldMatch` email

  when validateSAMLEmails $ do
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

  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

-- | create user with non-email externalId.  then use put to add an email address.
testScimUpdateEmailAddress :: (HasCallStack) => TaggedBool "extIdIsEmail" -> TaggedBool "validateSAMLEmails" -> App ()
testScimUpdateEmailAddress (TaggedBool extIdIsEmail) (TaggedBool validateSAMLEmails) = do
  (owner, tid, _) <- createTeam OwnDomain 1

  let status = if validateSAMLEmails then "enabled" else "disabled"
  assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" status

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, _) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"
  tok <-
    createScimToken owner (def {idp = Just idpId}) `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "token" >>= asString

  oldEmail <- randomEmail
  scimUser <-
    randomScimUserWith
      def
        { mkExternalId = if extIdIsEmail then pure oldEmail else randomUUIDString,
          prependExternalIdToEmails = False,
          mkOtherEmails = pure []
        }
  uid <- createScimUser owner tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

  getScimUser OwnDomain tok uid `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "id" `shouldMatch` uid
    lookupField res.json "emails"
      `shouldMatch` ( if extIdIsEmail
                        then Just [object ["value" .= oldEmail]]
                        else Nothing
                    )

  newEmail <- randomEmail
  let newScimUser =
        let addEmailsField :: Value -> Value
            addEmailsField = \case
              Object o ->
                Object
                  ( KeyMap.insert
                      (fromString "emails")
                      (toJSON [object ["value" .= newEmail]])
                      o
                  )
         in addEmailsField scimUser

  updateScimUser OwnDomain tok uid newScimUser `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail]]

  getScimUser OwnDomain tok uid `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail]]

  when validateSAMLEmails $ do
    getUsersId OwnDomain [uid] `bindResponse` \res -> do
      res.status `shouldMatchInt` 200
      user <- res.json >>= asList >>= assertOne
      user %. "status" `shouldMatch` "active"
      lookupField user "email" `shouldMatch` (Nothing :: Maybe String)

    getUsersByEmail OwnDomain [newEmail] `bindResponse` \res -> do
      res.status `shouldMatchInt` 200
      res.json >>= asList >>= shouldBeEmpty

    activateEmail OwnDomain newEmail

  getUsersId OwnDomain [uid] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail

  getUsersByEmail OwnDomain [newEmail] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail

-- | changing externalId and emails subsequently:
--
-- 1. create user with extid email;
-- 2. add email to emails field;
-- 3. change extId to uuid;
-- 4. change extId back to *other* email.
--
-- (may overlap with `testSsoLoginNoSamlEmailValidation`.)
testScimUpdateEmailAddressAndExternalId :: (HasCallStack) => App ()
testScimUpdateEmailAddressAndExternalId = do
  (owner, tid, _) <- createTeam OwnDomain 1

  let status = "disabled"
  assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" status

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, _) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"
  tok <-
    createScimToken owner (def {idp = Just idpId}) `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "token" >>= asString

  -- 1. create user with extid email
  extId1 <- randomEmail
  scimUser <-
    randomScimUserWith
      def
        { mkExternalId = pure extId1,
          prependExternalIdToEmails = False,
          mkOtherEmails = pure []
        }
  brigUserId <- createScimUser owner tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

  getScimUser OwnDomain tok brigUserId `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "id" `shouldMatch` brigUserId
    res.json %. "emails" `shouldMatch` [object ["value" .= extId1]]

  findUsersByExternalId OwnDomain tok extId1 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    (res.json %. "Resources") >>= asList >>= \[v] -> v %. "id" `shouldMatch` brigUserId

  -- 2. add email to emails field
  newEmail1 <- randomEmail
  let newScimUser1 =
        let addEmailsField :: Value -> Value
            addEmailsField = \case
              Object o ->
                Object
                  ( KeyMap.insert
                      (fromString "emails")
                      (toJSON [object ["value" .= newEmail1]])
                      o
                  )
         in addEmailsField scimUser

  updateScimUser OwnDomain tok brigUserId newScimUser1 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "externalId" `shouldMatch` extId1
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail1]]

  getScimUser OwnDomain tok brigUserId `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail1]]

  findUsersByExternalId OwnDomain tok extId1 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    (res.json %. "Resources") >>= asList >>= \[v] -> v %. "id" `shouldMatch` brigUserId

  getUsersId OwnDomain [brigUserId] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail1

  getUsersByEmail OwnDomain [newEmail1] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail1

  -- 3. change extId to uuid
  newExtId2 <- randomUUIDString
  let newScimUser2 =
        let updExtIdField :: Value -> Value
            updExtIdField = \case
              Object o -> Object (KeyMap.insert (fromString "externalId") (toJSON newExtId2) o)
         in updExtIdField newScimUser1

  updateScimUser OwnDomain tok brigUserId newScimUser2 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "externalId" `shouldMatch` newExtId2
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail1]]

  getScimUser OwnDomain tok brigUserId `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail1]]

  findUsersByExternalId OwnDomain tok newExtId2 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    (res.json %. "Resources") >>= asList >>= \[v] -> v %. "id" `shouldMatch` {- CRASH (list is empty) -} brigUserId

  getUsersId OwnDomain [brigUserId] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail1

  getUsersByEmail OwnDomain [newEmail1] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail1

  -- 4. change extId back to *other* email
  newEmail3 <- randomEmail
  let newScimUser3 =
        let updEmailExtId :: Value -> Value
            updEmailExtId = \case
              Object o -> Object (KeyMap.insert (fromString "externalId") (toJSON newEmail3) o)
         in updEmailExtId newScimUser2

  updateScimUser OwnDomain tok brigUserId newScimUser3 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "externalId" `shouldMatch` newEmail3
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail1]]

  getScimUser OwnDomain tok brigUserId `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "emails" `shouldMatch` [object ["value" .= newEmail1]]

  findUsersByExternalId OwnDomain tok newEmail3 `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    (res.json %. "Resources") >>= asList >>= \[v] -> v %. "id" `shouldMatch` brigUserId

  getUsersId OwnDomain [brigUserId] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail1

  getUsersByEmail OwnDomain [newEmail1] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` newEmail1

testScimLoginNoSamlEmailValidation :: (HasCallStack) => TaggedBool "validateSAMLEmails" -> App ()
testScimLoginNoSamlEmailValidation (TaggedBool validateSAMLEmails) = do
  (owner, tid, _) <- createTeam OwnDomain 1

  let status = if validateSAMLEmails then "enabled" else "disabled"
  assertSuccess =<< setTeamFeatureStatus owner tid "validateSAMLemails" status

  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  (idp, _) <- registerTestIdPWithMetaWithPrivateCreds owner
  idpId <- asString $ idp.json %. "id"
  tok <-
    createScimToken owner (def {idp = Just idpId}) `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "token" >>= asString

  scimUser <- randomScimUser
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  uid <- createScimUser owner tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

  getScimUser OwnDomain tok uid `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "id" `shouldMatch` uid

  when validateSAMLEmails $ do
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

  getUsersByEmail OwnDomain [email] `bindResponse` \res -> do
    res.status `shouldMatchInt` 200
    user <- res.json >>= asList >>= assertOne
    user %. "status" `shouldMatch` "active"
    user %. "email" `shouldMatch` email

testIdpUpdate :: (HasCallStack) => App ()
testIdpUpdate = do
  (owner, tid, []) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  -- register an IdP
  idp@(idpId, (idpmeta, pCreds)) <- do
    (resp, meta) <- registerTestIdPWithMetaWithPrivateCreds owner
    (,meta) <$> asString (resp.json %. "id")
  -- create a SCIM token
  tok <-
    createScimToken owner (def {idp = Just idpId}) `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "token" >>= asString
  -- create SCIM users
  uids <- replicateM 3 $ do
    scimUser <- randomScimUser
    email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
    uid <- createScimUser owner tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
    void $ loginWithSamlEmail True tid email idp
    activateEmail OwnDomain email
    getScimUser OwnDomain tok uid `bindResponse` \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "id" `shouldMatch` uid
    pure (uid, email)
  -- update the IdP
  idp2 <- do
    (resp, meta) <- updateTestIdpWithMetaWithPrivateCreds owner idpId
    (,meta) <$> asString (resp.json %. "id")
  -- the SCIM users can login
  for_ uids $ \(_, email) -> do
    void $ loginWithSamlEmail True tid email idp2
  -- update the IdP again and use the original metadata
  idp3 <- do
    resp <- updateIdp owner idpId idpmeta
    (,(idpmeta, pCreds)) <$> asString (resp.json %. "id")
  -- the SCIM users can still login
  for_ uids $ \(_, email) -> do
    void $ loginWithSamlEmail True tid email idp3

-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- Allow updates of E2EI enabled users only via SCIM
testAllowUpdatesBySCIMWhenE2EIdEnabled :: (HasCallStack) => TaggedBool "sso-enabled" -> App ()
testAllowUpdatesBySCIMWhenE2EIdEnabled (TaggedBool ssoEnabled) = do
  (tok, uid, su) <- if ssoEnabled then setupWithSSO else setupWithoutSSO
  user <- getUsersId OwnDomain [uid] >>= getJSON 200 >>= asList >>= assertOne

  checkUpdateHandleByUserFails user
  su1 <- checkUpdateHandleByScimSucceeds tok uid su
  checkUpdateDisplayNameByUserFails user
  su2 <- checkUpdateDisplayNameByScimSucceeds tok uid su1

  -- the following should not be part of the e2eid certification, but are checked here anyway
  checkUpdateLocaleByUserFails user
  su3 <- checkUpdateLocaleByScimSucceeds tok uid su2
  unless ssoEnabled $ checkUpdateEmailByUserFails user
  su4 <- checkUpdateEmailByScimSucceeds tok uid su3
  -- external ID cannot be updated by the user, only by SCIM
  void $ checkUpdateExternalIdByScimSucceeds tok uid su4
  where
    setupWithSSO :: App (String, String, Value)
    setupWithSSO = do
      (owner, tid, _) <- createTeam OwnDomain 1
      setTeamFeatureStatus owner tid "sso" "enabled" >>= assertSuccess
      setTeamFeatureStatus owner tid "mlsE2EId" "enabled" >>= assertSuccess
      void $ registerTestIdPWithMeta owner >>= getJSON 201
      tok <- createScimTokenV6 owner def >>= getJSON 200 >>= (%. "token") >>= asString
      scimUser <- randomScimUser
      email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
      uid <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
      activateEmail OwnDomain email
      pure (tok, uid, scimUser)

    setupWithoutSSO :: App (String, String, Value)
    setupWithoutSSO = do
      (owner, tid, _) <- createTeam OwnDomain 1
      setTeamFeatureStatus owner tid "mlsE2EId" "enabled" >>= assertSuccess
      tok <- createScimTokenV6 owner def >>= getJSON 200 >>= (%. "token") >>= asString
      scimUser <- randomScimUser
      email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
      uid <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
      registerInvitedUser OwnDomain tid email
      pure (tok, uid, scimUser)

    checkUpdateHandleByScimSucceeds :: (HasCallStack) => String -> String -> Value -> App Value
    checkUpdateHandleByScimSucceeds tok uid scimUser = do
      newHandle <- randomHandle
      su <- setField "userName" newHandle scimUser
      bindResponse (updateScimUser OwnDomain tok uid su) $ \res -> do
        res.status `shouldMatchInt` 200
        res.json %. "userName" `shouldMatch` newHandle
      bindResponse (getUsersId OwnDomain [uid]) $ \res -> do
        res.status `shouldMatchInt` 200
        u <- res.json >>= asList >>= assertOne
        u %. "handle" `shouldMatch` newHandle
      pure su

    checkUpdateHandleByUserFails :: (HasCallStack, MakesValue user) => user -> App ()
    checkUpdateHandleByUserFails user = do
      putHandle user "new-handle" `bindResponse` \res -> do
        res.status `shouldMatchInt` 403
        res.json %. "label" `shouldMatch` "managed-by-scim"

    checkUpdateDisplayNameByScimSucceeds :: (HasCallStack) => String -> String -> Value -> App Value
    checkUpdateDisplayNameByScimSucceeds tok uid scimUser = do
      let displayName = "Alice in Wonderland"
      su <- setField "displayName" displayName scimUser
      bindResponse (updateScimUser OwnDomain tok uid su) $ \res -> do
        res.status `shouldMatchInt` 200
        res.json %. "displayName" `shouldMatch` displayName
      bindResponse (getUsersId OwnDomain [uid]) $ \res -> do
        res.status `shouldMatchInt` 200
        u <- res.json >>= asList >>= assertOne
        u %. "name" `shouldMatch` displayName
      pure su

    checkUpdateDisplayNameByUserFails :: (HasCallStack, MakesValue user) => user -> App ()
    checkUpdateDisplayNameByUserFails user = do
      putSelf user def {name = Just "Bob the Builder"} `bindResponse` \res -> do
        res.status `shouldMatchInt` 403
        res.json %. "label" `shouldMatch` "managed-by-scim"

    checkUpdateLocaleByScimSucceeds :: (HasCallStack) => String -> String -> Value -> App Value
    checkUpdateLocaleByScimSucceeds tok uid scimUser = do
      su <- setField "preferredLanguage" "fr" scimUser
      bindResponse (updateScimUser OwnDomain tok uid su) $ \res -> do
        res.status `shouldMatchInt` 200
        res.json %. "preferredLanguage" `shouldMatch` "fr"
      bindResponse (getUsersId OwnDomain [uid]) $ \res -> do
        res.status `shouldMatchInt` 200
        u <- res.json >>= asList >>= assertOne
        u %. "locale" `shouldMatch` "fr"
      pure su

    checkUpdateLocaleByUserFails :: (HasCallStack, MakesValue user) => user -> App ()
    checkUpdateLocaleByUserFails user = do
      putSelfLocale user "de" `bindResponse` \res -> do
        res.status `shouldMatchInt` 403
        res.json %. "label" `shouldMatch` "managed-by-scim"

    checkUpdateEmailByScimSucceeds :: (HasCallStack) => String -> String -> Value -> App Value
    checkUpdateEmailByScimSucceeds tok uid scimUser = do
      newEmail <- randomEmail
      su <- setField "emails" [object ["value" .= newEmail]] scimUser
      bindResponse (updateScimUser OwnDomain tok uid su) $ \res -> do
        res.status `shouldMatchInt` 200
        res.json %. "emails" `shouldMatch` [object ["value" .= newEmail]]
      activateEmail OwnDomain newEmail
      bindResponse (getUsersId OwnDomain [uid]) $ \res -> do
        res.status `shouldMatchInt` 200
        u <- res.json >>= asList >>= assertOne
        u %. "email" `shouldMatch` newEmail
      pure su

    checkUpdateEmailByUserFails :: (HasCallStack, MakesValue user) => user -> App ()
    checkUpdateEmailByUserFails user = do
      email <- make user %. "email" >>= asString
      (cookie, token) <-
        login OwnDomain email defPassword `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          token <- resp.json %. "access_token" & asString
          let cookie = fromJust $ getCookie "zuid" resp
          pure ("zuid=" <> cookie, token)
      newEmail <- randomEmail
      updateEmail user newEmail cookie token `bindResponse` \res -> do
        res.status `shouldMatchInt` 403
        res.json %. "label" `shouldMatch` "managed-by-scim"

    checkUpdateExternalIdByScimSucceeds :: (HasCallStack) => String -> String -> Value -> App Value
    checkUpdateExternalIdByScimSucceeds tok uid scimUser = do
      newExtId <- randomUUIDString
      su <- setField "externalId" newExtId scimUser
      bindResponse (updateScimUser OwnDomain tok uid su) $ \res -> do
        res.status `shouldMatchInt` 200
        res.json %. "externalId" `shouldMatch` newExtId
      bindResponse (getUsersId OwnDomain [uid]) $ \res -> do
        res.status `shouldMatchInt` 200
        u <- res.json >>= asList >>= assertOne
        subject <-
          if ssoEnabled
            then
              u %. "sso_id.subject" >>= asString
            else
              u %. "sso_id.scim_external_id" >>= asString
        subject `shouldContainString` newExtId
      pure su

-- @END
