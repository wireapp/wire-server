{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import qualified API.Brig as Brig
import API.BrigInternal as BrigInternal
import API.Common (randomEmail, randomExternalId, randomHandle)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar
import Control.Concurrent (threadDelay)
import Data.Vector (fromList)
import qualified Data.Vector as Vector
import SAML2.WebSSO.Test.Util (SampleIdP (..), makeSampleIdPMetadata)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

testSparUserCreationInvitationTimeout :: (HasCallStack) => App ()
testSparUserCreationInvitationTimeout = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

  -- Trying to create the same user again right away should fail
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 409

  -- However, if we wait until the invitation timeout has passed
  -- (assuming it is configured to 10s locally and in CI)...
  liftIO $ threadDelay (11_000_000)

  -- ...we should be able to create the user again
  retryT $ bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

testSparExternalIdDifferentFromEmailWithIdp :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmailWithIdp = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  void $ registerTestIdPWithMeta owner >>= getJSON 201
  tok <- createScimToken owner >>= getJSON 200 >>= (%. "token") >>= asString
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

registerTestIdPWithMeta :: (HasCallStack, MakesValue owner) => owner -> App Response
registerTestIdPWithMeta owner = do
  SampleIdP idpmeta _ _ _ <- makeSampleIdPMetadata
  createIdp owner idpmeta

testSparExternalIdDifferentFromEmail :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
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
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
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
testSparMigrateFromExternalIdOnlyToEmail emailUnchanged = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
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
  (key, code) <- bindResponse (BrigInternal.getActivationCode domain email) $ \res -> do
    (,)
      <$> (res.json %. "key" >>= asString)
      <*> (res.json %. "code" >>= asString)
  Brig.activate domain key code >>= assertSuccess

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
