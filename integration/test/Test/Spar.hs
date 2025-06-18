{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import API.Brig as Brig
import API.BrigInternal as BrigInternal
import API.Common (randomDomain, randomEmail, randomExternalId, randomHandle)
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
import Data.Vector (fromList)
import qualified Data.Vector as Vector
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
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
    updatedScimUser <- setField "emails" (Array (Vector.fromList [object ["value" .= newEmail]])) scimUserWith2Updates
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
    updatedScimUser <- setField "emails" (Array (Vector.fromList [object ["value" .= newEmail]])) scimUserWith2Updates
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
    resp.json %. "emails" `shouldMatch` (Array (fromList [object ["value" .= email]]))
    resp.status `shouldMatchInt` 200

  newEmail <- if emailUnchanged then pure email else randomEmail
  let newEmails = (Array (fromList [object ["value" .= newEmail]]))
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
                      (Array (fromList [object ["value" .= newEmail]]))
                      o
                  )
              v -> v
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
