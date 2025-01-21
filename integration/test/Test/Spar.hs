{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import qualified API.Brig as Brig
import API.BrigInternal as BrigInternal
import API.Common (randomEmail, randomExternalId, randomHandle)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar
import API.SparInternal
import Control.Concurrent (threadDelay)
import Data.Vector (fromList)
import qualified Data.Vector as Vector
import SetupHelpers
import Testlib.JSON
import Testlib.PTest
import Testlib.Prelude

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
  scimUser <- randomScimUserWith extId email
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
  scimUser <- randomScimUserWith extId email
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json >>= asList >>= shouldBeEmpty

  registerUser OwnDomain tid email

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
  registerUser OwnDomain tid email

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
  registerUser OwnDomain tid email

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

registerUser :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App ()
registerUser domain tid email = do
  BrigInternal.getInvitationByEmail domain email
    >>= getJSON 200
    >>= BrigInternal.getInvitationCodeForTeam domain tid
    >>= getJSON 200
    >>= (%. "code")
    >>= asString
    >>= Brig.registerUser domain email
    >>= assertSuccess

activateEmail :: (HasCallStack, MakesValue domain) => domain -> String -> App ()
activateEmail domain email = do
  (actkey, code) <- bindResponse (BrigInternal.getActivationCode domain email) $ \res -> do
    (,)
      <$> (res.json %. "key" >>= asString)
      <*> (res.json %. "code" >>= asString)
  Brig.activate domain actkey code >>= assertSuccess

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
