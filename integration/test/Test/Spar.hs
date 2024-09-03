{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import qualified API.Brig as Brig
import API.BrigInternal as BrigInternal
import API.Common (randomEmail, randomExternalId, randomHandle)
import API.Spar
import Control.Concurrent (threadDelay)
import Data.Vector (fromList)
import qualified Data.Vector as Vector
import SAML2.WebSSO as SAML hiding ((<$$>))
import qualified SAML2.WebSSO.API.Example as SAML
import SAML2.WebSSO.Test.Lenses (userRefL)
import SAML2.WebSSO.Test.MockResponse
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
  (_owner, _tid, _) <- createTeam OwnDomain 1
  pure ()

-- | Call 'registerTestIdP', then 'registerScimToken'.  The user returned is the owner of the team;
-- the IdP is registered with the team; the SCIM token can be used to manipulate the team.
registerIdPAndScimToken :: (HasCallStack, MakesValue owner) => owner -> App ()
registerIdPAndScimToken owner = do
  idp <- registerTestIdPWithMeta owner
  pure ()

-- token <- registerScimToken teamid (Just (idp ^. idpId))
-- let team = (owner, teamid, idp)
-- pure (token, team)

-- | Create a fresh 'IdPMetadata' suitable for testing.
registerTestIdPWithMeta :: (HasCallStack, MakesValue owner) => owner -> App (Value, (Value, String))
registerTestIdPWithMeta owner = do
  SampleIdP idpmeta privkey _ _ <- makeSampleIdPMetadata
  undefined

-- env <- ask
-- idp <- registerTestIdPFrom idpmeta (env ^. teMgr) owner (env ^. teSpar)
-- pure (idp, (IdPMetadataValue (cs $ SAML.encode idpmeta) idpmeta, privkey))

testSparExternalIdDifferentFromEmail :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
  email <- randomEmail
  extId <- randomExternalId
  scimUser <- randomScimUserWith extId email
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  registerUser OwnDomain tid email
  bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json %. "Resources" >>= asList >>= assertOne
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
    bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json %. "Resources" >>= asList >>= assertOne
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
    bindResponse (findUsersByExternalId OwnDomain tok newExtId) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json %. "Resources" >>= asList >>= assertOne
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

    -- before activation the old email should still be present
    bindResponse (findUsersByExternalId OwnDomain tok currentExtId) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json %. "Resources" >>= asList >>= assertOne
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` oldEmail
      u %. "sso_id.scim_external_id" `shouldMatch` currentExtId

    -- after activation the new email should be present
    activateEmail OwnDomain newEmail
    bindResponse (findUsersByExternalId OwnDomain tok currentExtId) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json %. "Resources" >>= asList >>= assertOne
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
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  registerUser OwnDomain tid email

  let extId = "notanemailaddress"
  updatedScimUser <- setField "externalId" extId scimUser
  updateScimUser OwnDomain tok userId updatedScimUser >>= assertStatus 400

testSparMigrateFromExternalIdOnlyToEmail :: (HasCallStack) => Bool -> App ()
testSparMigrateFromExternalIdOnlyToEmail emailUnchanged = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser >>= removeField "emails"
  email <- scimUser %. "externalId" >>= asString
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  registerUser OwnDomain tid email

  newEmail <- if emailUnchanged then pure email else randomEmail
  let newEmails = (Array (fromList [object ["value" .= newEmail]]))
  updatedScimUser <-
    setField
      "emails"
      newEmails
      scimUser
  updateScimUser OwnDomain tok userId updatedScimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "externalId" `shouldMatch` (updatedScimUser %. "externalId")
    resp.json %. "emails" `shouldMatch` (updatedScimUser %. "emails")

  -- after activation the new email should be present
  unless emailUnchanged $ activateEmail OwnDomain newEmail

  extId <- scimUser %. "externalId" >>= asString
  bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json %. "Resources" >>= asList >>= assertOne
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
