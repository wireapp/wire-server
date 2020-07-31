{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Integration tests for the branch of SCIM API that deals with users (@\/scim\/v2\/Users@).
module Test.Spar.Scim.UserSpec
  ( spec,
  )
where

import Bilge
import Bilge.Assert
import Brig.Types.Intra (AccountStatus (Active, Suspended))
import Brig.Types.User as Brig
import Control.Lens
import Control.Monad.Catch (MonadCatch)
import Control.Retry (exponentialBackoff, limitRetries, recovering)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (fromJSON, toJSON)
import Data.ByteString.Conversion
import Data.Handle (Handle (Handle), fromHandle)
import Data.Id (TeamId, UserId, randomId)
import Data.Ix (inRange)
import Data.String.Conversions (cs)
import qualified Data.Text.Ascii as Ascii
import Imports
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import qualified SAML2.WebSSO.Types as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import Spar.Scim
import Spar.Types (IdP)
import qualified Spar.Types
import qualified Text.XML.DSig as SAML
import Util
import qualified Web.Scim.Class.User as Scim.UserC
import qualified Web.Scim.Filter as Filter
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.PatchOp as PatchOp
import qualified Web.Scim.Schema.User as Scim.User
import qualified Wire.API.Team.Feature as Feature
import qualified Wire.API.User.Activation as Activation
import Wire.API.User.RichInfo

-- | Tests for @\/scim\/v2\/Users@.
spec :: SpecWith TestEnv
spec = do
  specCreateUser
  specListUsers
  specGetUser
  specPatchUser
  specUpdateUser
  specDeleteUser
  specAzureQuirks
  specEmailValidation
  specSuspend
  describe "CRUD operations maintain invariants in mapScimToBrig, mapBrigToScim." $ do
    it "..." $ do
      pendingWith "this is a job for quickcheck-state-machine"
  describe "validateScimUser'" $ do
    it "works" $ do
      pendingWith "write a list of unit tests here that make the mapping explicit, exhaustive, and easy to read."

specSuspend :: SpecWith TestEnv
specSuspend = do
  describe "suspend" $ do
    let checkPreExistingUser :: Bool -> TestSpar ()
        checkPreExistingUser isActive = do
          (_, teamid, idp, (_, privCreds)) <- registerTestIdPWithMeta
          member <- loginSsoUserFirstTime idp privCreds
          -- NOTE: once SCIM is enabled SSO Auto-provisioning is disabled
          tok <- registerScimToken teamid (Just (idp ^. SAML.idpId))
          handle'@(Handle handle) <- nextHandle
          runSpar $ Intra.setBrigUserHandle member handle'
          unless isActive $ do
            runSpar $ Intra.setStatus member Suspended
          [user] <- listUsers tok (Just (filterBy "userName" handle))
          lift $ (Scim.User.active . Scim.value . Scim.thing $ user) `shouldBe` Just isActive
    it "pre-existing suspended users are inactive" $ do
      checkPreExistingUser False
    it "pre-existing unsuspended users are active" $ do
      checkPreExistingUser True

    let activeInactiveAndBack putOrPatch = do
          user <- randomScimUser
          (tok, _) <- registerIdPAndScimToken
          scimStoredUserBlah <- createUser tok user
          let uid = Scim.id . Scim.thing $ scimStoredUserBlah
          do
            -- NOTE: It's Nothing, not Just True, as we want to also test the fact that existing
            -- SCIM records don't have the active field. absence of active should be interpreted as Active.
            -- Once we get rid of the `scim` table and make scim serve brig records directly, this is
            -- not an issue anymore.
            lift $ (Scim.User.active . Scim.value . Scim.thing $ scimStoredUserBlah) `shouldBe` Just True
            void $ aFewTimes (runSpar $ Intra.getStatus uid) (== Active)
          do
            scimStoredUser <- putOrPatch tok uid user True
            lift $ (Scim.User.active . Scim.value . Scim.thing $ scimStoredUser) `shouldBe` Just True
            void $ aFewTimes (runSpar $ Intra.getStatus uid) (== Active)
          do
            scimStoredUser <- putOrPatch tok uid user False
            lift $ (Scim.User.active . Scim.value . Scim.thing $ scimStoredUser) `shouldBe` Just False
            void $ aFewTimes (runSpar $ Intra.getStatus uid) (== Suspended)
          do
            scimStoredUser <- putOrPatch tok uid user True
            lift $ (Scim.User.active . Scim.value . Scim.thing $ scimStoredUser) `shouldBe` Just True
            void $ aFewTimes (runSpar $ Intra.getStatus uid) (== Active)

    it "PUT will change state from active to inactive and back" $ do
      void . activeInactiveAndBack $ \tok uid user active ->
        updateUser tok uid user {Scim.User.active = Just active}

    it "PATCH will change state from active to inactive and back" $ do
      let replaceAttrib name value =
            PatchOp.Operation
              PatchOp.Replace
              (Just (PatchOp.NormalPath (Filter.topLevelAttrPath name)))
              (Just (toJSON value))
      void . activeInactiveAndBack $ \tok uid _user active ->
        patchUser tok uid $ PatchOp.PatchOp [replaceAttrib "active" active]

    -- Consider the following series of events:
    --
    -- ```
    -- { }                 --- patch "active" true --->
    -- { "active": true }  --- patch "active" false --->
    -- { "active": false } --- delete "active" --->
    -- { }
    -- ```
    --
    -- Since we give the case of missing active flag the same meaning as the flag set to
    -- @True@, it's most consistent to also re-activating a suspended user if the active flag
    -- is removed: the active flag must have been @False@ before (otherwise the user would
    -- have been active already), and if we didn't re-activate the user, the next scim-get
    -- would yield @{ "active": false }@, which is plainly wrong.
    it "PATCH removing the active attribute makes you active" $ do
      let deleteAttrib name =
            PatchOp.Operation
              PatchOp.Remove
              (Just (PatchOp.NormalPath (Filter.topLevelAttrPath name)))
              Nothing
      user <- randomScimUser
      (tok, _) <- registerIdPAndScimToken
      scimStoredUserBlah <- createUser tok user
      let uid = Scim.id . Scim.thing $ scimStoredUserBlah
      runSpar $ Intra.setStatus uid Suspended
      void $ aFewTimes (runSpar $ Intra.getStatus uid) (== Suspended)
      void $ patchUser tok uid $ PatchOp.PatchOp [deleteAttrib "active"]
      void $ aFewTimes (runSpar $ Intra.getStatus uid) (== Active)

----------------------------------------------------------------------------
-- User creation

-- | Tests for @POST /Users@.
specCreateUser :: SpecWith TestEnv
specCreateUser = describe "POST /Users" $ do
  it "creates a user in an existing team" $ testCreateUser
  it "adds a Wire scheme to the user record" $ testSchemaIsAdded
  it "requires externalId to be present" $ testExternalIdIsRequired
  it "rejects invalid handle" $ testCreateRejectsInvalidHandle
  it "rejects occupied handle" $ testCreateRejectsTakenHandle
  it "rejects occupied externalId" $ testCreateRejectsTakenExternalId
  it "allows an occupied externalId when the IdP is different" $
    testCreateSameExternalIds
  it "provides a correct location in the 'meta' field" $ testLocation
  it "handles rich info correctly (this also tests put, get)" $ testRichInfo
  it "gives created user a valid 'SAML.UserRef' for SSO" $ testScimCreateVsUserRef
  it "attributes of {brig, scim, saml} user are mapped as documented" $ pending
  it "writes all the stuff to all the places" $
    pendingWith "factor this out of the PUT tests we already wrote."

-- | Test that a user can be created via SCIM and that it also appears on the Brig side.
testCreateUser :: TestSpar ()
testCreateUser = do
  env <- ask
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  scimStoredUser <- createUser tok user
  let userid = scimUserId scimStoredUser
  -- Check that this user is present in Brig and that Brig's view of the user
  -- matches SCIM's view of the user
  brigUser :: User <-
    fmap responseJsonUnsafe . call . get $
      ( (env ^. teBrig)
          . header "Z-User" (toByteString' userid)
          . path "/self"
          . expect2xx
      )
  brigUser `userShouldMatch` WrappedScimStoredUser scimStoredUser

-- | Test that Wire-specific schemas are added to the SCIM user record, even if the schemas
-- were not present in the original record during creation.
testSchemaIsAdded :: TestSpar ()
testSchemaIsAdded = do
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  scimStoredUser <- createUser tok (user {Scim.User.schemas = []})
  -- Check that the created user has the right schemas
  liftIO $
    Scim.User.schemas (Scim.value (Scim.thing scimStoredUser))
      `shouldBe` userSchemas

-- | Test that @externalId@ (for SSO login) is required when creating a user.
testExternalIdIsRequired :: TestSpar ()
testExternalIdIsRequired = do
  env <- ask
  -- Create a user with a missing @externalId@ and check that it fails
  user <- randomScimUser
  let user' = user {Scim.User.externalId = Nothing}
  (tok, _) <- registerIdPAndScimToken
  createUser_ (Just tok) user' (env ^. teSpar)
    !!! const 400 === statusCode

-- | Test that user creation fails if handle is invalid
testCreateRejectsInvalidHandle :: TestSpar ()
testCreateRejectsInvalidHandle = do
  env <- ask
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  createUser_ (Just tok) (user {Scim.User.userName = "#invalid name"}) (env ^. teSpar)
    !!! const 400 === statusCode

-- | Test that user creation fails if handle is already in use (even on different team).
testCreateRejectsTakenHandle :: TestSpar ()
testCreateRejectsTakenHandle = do
  env <- ask
  user1 <- randomScimUser
  user2 <- randomScimUser
  user3 <- randomScimUser
  (tokTeamA, _) <- registerIdPAndScimToken
  (tokTeamB, _) <- registerIdPAndScimToken
  -- Create and add a first user: success!
  _ <- createUser tokTeamA user1
  -- Try to create different user with same handle in same team.
  createUser_ (Just tokTeamA) (user2 {Scim.User.userName = Scim.User.userName user1}) (env ^. teSpar)
    !!! const 409 === statusCode
  -- Try to create different user with same handle in different team.
  createUser_ (Just tokTeamB) (user3 {Scim.User.userName = Scim.User.userName user1}) (env ^. teSpar)
    !!! const 409 === statusCode

-- | Test that user creation fails if the @externalId@ is already in use for given IdP.
testCreateRejectsTakenExternalId :: TestSpar ()
testCreateRejectsTakenExternalId = do
  env <- ask
  (tok, _) <- registerIdPAndScimToken
  -- Create and add a first user: success!
  user1 <- randomScimUser
  _ <- createUser tok user1
  -- Try to create different user with same @externalId@ in same team, and fail.
  user2 <- randomScimUser
  createUser_ (Just tok) (user2 {Scim.User.externalId = Scim.User.externalId user1}) (env ^. teSpar)
    !!! const 409 === statusCode

-- | Test that it's fine to have same @externalId@s for two users belonging to different IdPs.
testCreateSameExternalIds :: TestSpar ()
testCreateSameExternalIds = do
  -- Create and add a first user: success!
  (tokTeamA, _) <- registerIdPAndScimToken
  user1 <- randomScimUser
  _ <- createUser tokTeamA user1
  -- Create different user with same @externalId@ in a different team (which by necessity
  -- has a different IdP)
  (tokTeamB, _) <- registerIdPAndScimToken
  user2 <- randomScimUser
  _ <- createUser tokTeamB (user2 {Scim.User.externalId = Scim.User.externalId user1})
  pure ()

-- | Test that the resource location returned for the user is correct and the user can be
-- fetched by following that location.
--
-- TODO: also check the @Location@ header. Currently we don't set the @Location@ header, but
-- we should.
testLocation :: TestSpar ()
testLocation = do
  -- Create a user
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  scimStoredUser <- createUser tok user
  -- Fetch the @meta.location@ and check that it returns the same user
  let location = Scim.location (Scim.meta scimStoredUser)
  req <-
    parseRequest (show (Scim.unURI location))
      <&> scimAuth (Just tok) . acceptScim
  r <- call (get (const req)) <!! const 200 === statusCode
  liftIO $ responseJsonUnsafe r `shouldBe` scimStoredUser

testRichInfo :: TestSpar ()
testRichInfo = do
  let richInfo = RichInfo (RichInfoAssocList [RichField "Platforms" "OpenBSD; Plan9"])
      richInfoOverwritten = RichInfo (RichInfoAssocList [RichField "Platforms" "Windows10"])
      richInfoPatchedMap = RichInfo (RichInfoAssocList [RichField "Platforms" "Arch, BTW"])
      richInfoPatchedList = RichInfo (RichInfoAssocList [RichField "Platforms" "none"])
      (Aeson.Success patchOpMap) =
        fromJSON
          [aesonQQ|{
                                                      "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                                      "operations" : [{
                                                        "op" : "replace",
                                                        "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:Platforms",
                                                        "value" : "Arch, BTW"
                                                      }]
                                                    }|]
      (Aeson.Success patchOpList) =
        fromJSON
          [aesonQQ|{
                                                      "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                                      "operations" : [{
                                                        "op" : "replace",
                                                        "path" : "urn:wire:scim:schemas:profile:1.0:Platforms",
                                                        "value" : "none"
                                                      }]
                                                    }|]

  brig <- asks (view teBrig)
  -- set things up
  (user, _) <- randomScimUserWithSubjectAndRichInfo richInfo
  (userOverwritten, _) <- randomScimUserWithSubjectAndRichInfo richInfoOverwritten
  (tok, (owner, _, _)) <- registerIdPAndScimToken
  let -- validate response
      checkStoredUser ::
        HasCallStack =>
        Scim.UserC.StoredUser SparTag ->
        RichInfo ->
        TestSpar ()
      checkStoredUser storedUser rinf = liftIO $ do
        (Scim.User.extra . Scim.value . Scim.thing) storedUser
          `shouldBe` ScimUserExtra rinf
      -- validate server state after the fact
      probeUser ::
        HasCallStack =>
        UserId ->
        RichInfo ->
        TestSpar ()
      probeUser uid rinf = do
        -- get scim user yields correct rich info.
        scimStoredUser' <- getUser tok uid
        checkStoredUser scimStoredUser' rinf
        -- get rich info end-point on brig yields correct rich info.
        resp <-
          call $
            get
              ( brig
                  . paths ["users", toByteString' uid, "rich-info"]
                  . zUser owner
              )
        liftIO $ do
          statusCode resp `shouldBe` 200
          responseJsonEither resp `shouldBe` Right (unRichInfo rinf)
  -- post response contains correct rich info.
  postResp :: Scim.UserC.StoredUser SparTag <- createUser tok user
  let postUid = scimUserId postResp
  checkStoredUser postResp richInfo
  -- post updates the backend as expected.
  probeUser postUid richInfo
  -- put response contains correct rich info.
  putResp :: Scim.UserC.StoredUser SparTag <- updateUser tok postUid userOverwritten
  let putUid = scimUserId putResp
  checkStoredUser putResp richInfoOverwritten
  -- put updates the backend as expected.
  liftIO $ putUid `shouldBe` postUid
  probeUser putUid richInfoOverwritten
  -- patch response contains correct rich info.
  do
    -- patch via map schema
    patchResp :: Scim.UserC.StoredUser SparTag <- patchUser tok postUid patchOpMap
    let patchUid = scimUserId patchResp
    checkStoredUser patchResp richInfoPatchedMap
    -- patch updates the backend as expected.
    liftIO $ patchUid `shouldBe` postUid
    probeUser patchUid richInfoPatchedMap
  do
    -- patch via list schema (deprecated, probably)
    patchResp :: Scim.UserC.StoredUser SparTag <- patchUser tok postUid patchOpList
    let patchUid = scimUserId patchResp
    checkStoredUser patchResp richInfoPatchedList
    -- patch updates the backend as expected.
    liftIO $ patchUid `shouldBe` postUid
    probeUser patchUid richInfoPatchedList

-- | Create a user implicitly via saml login; remove it via brig leaving a dangling entry in
-- @spar.user@; create it via scim.  This should work despite the dangling database entry.
testScimCreateVsUserRef :: TestSpar ()
testScimCreateVsUserRef = do
  (_ownerid, teamid, idp, (_, privCreds)) <- registerTestIdPWithMeta
  (usr, uname) :: (Scim.User.User SparTag, SAML.UnqualifiedNameID) <-
    randomScimUserWithSubject
  let uref = SAML.UserRef tenant subj
      subj = either (error . show) id $ SAML.mkNameID uname Nothing Nothing Nothing
      tenant = idp ^. SAML.idpMetadata . SAML.edIssuer
  !(Just !uid) <- createViaSaml idp privCreds uref
  samlUserShouldSatisfy uref isJust
  deleteViaBrig uid
  samlUserShouldSatisfy uref isJust -- brig doesn't talk to spar right now when users
  -- are deleted there.  we need to work around this
  -- fact for now.  (if the test fails here, this may
  -- mean that you fixed the behavior and can
  -- change this to 'isNothing'.)
  tok <- registerScimToken teamid (Just (idp ^. SAML.idpId))
  storedusr :: Scim.UserC.StoredUser SparTag <-
    do
      resp <-
        aFewTimes (createUser_ (Just tok) usr =<< view teSpar) ((== 201) . statusCode)
          <!! const 201 === statusCode
      pure $ responseJsonUnsafe resp
  samlUserShouldSatisfy uref (== Just (scimUserId storedusr))
  -- now with a scim token in the team, we can't auto-provision via saml any more.
  (_usr', uname') :: (Scim.User.User SparTag, SAML.UnqualifiedNameID) <-
    randomScimUserWithSubject
  let uref' = SAML.UserRef tenant' subj'
      subj' = either (error . show) id $ SAML.mkNameID uname' Nothing Nothing Nothing
      tenant' = idp ^. SAML.idpMetadata . SAML.edIssuer
  createViaSamlFails idp privCreds uref'
  where
    samlUserShouldSatisfy :: HasCallStack => SAML.UserRef -> (Maybe UserId -> Bool) -> TestSpar ()
    samlUserShouldSatisfy uref property = do
      muid <- getUserIdViaRef' uref
      liftIO $ muid `shouldSatisfy` property
    createViaSamlResp :: HasCallStack => IdP -> SAML.SignPrivCreds -> SAML.UserRef -> TestSpar ResponseLBS
    createViaSamlResp idp privCreds (SAML.UserRef _ subj) = do
      authnReq <- negotiateAuthnRequest idp
      spmeta <- getTestSPMetadata
      authnResp <-
        runSimpleSP $
          SAML.mkAuthnResponseWithSubj subj privCreds idp spmeta authnReq True
      submitAuthnResponse authnResp <!! const 200 === statusCode
    createViaSamlFails :: HasCallStack => IdP -> SAML.SignPrivCreds -> SAML.UserRef -> TestSpar ()
    createViaSamlFails idp privCreds uref = do
      resp <- createViaSamlResp idp privCreds uref
      liftIO $ do
        maybe (error "no body") cs (responseBody resp)
          `shouldContain` "<title>wire:sso:error:forbidden</title>"
    createViaSaml :: HasCallStack => IdP -> SAML.SignPrivCreds -> SAML.UserRef -> TestSpar (Maybe UserId)
    createViaSaml idp privCreds uref = do
      resp <- createViaSamlResp idp privCreds uref
      liftIO $ do
        maybe (error "no body") cs (responseBody resp)
          `shouldContain` "<title>wire:sso:success</title>"
      getUserIdViaRef' uref
    deleteViaBrig :: UserId -> TestSpar ()
    deleteViaBrig uid = do
      brig <- view teBrig
      (call . delete $ brig . paths ["i", "users", toByteString' uid])
        !!! const 202 === statusCode

----------------------------------------------------------------------------
-- Listing users

-- | Tests for @GET /Users@.
specListUsers :: SpecWith TestEnv
specListUsers = describe "GET /Users" $ do
  it "lists all SCIM users in a team" $ testListProvisionedUsers
  it "finds a SCIM-provisioned user by userName or externalId" $ testFindProvisionedUser
  it "finds a non-SCIM-provisioned user by userName or externalId" $ testFindNonProvisionedUser
  it "doesn't list deleted users" $ testListNoDeletedUsers
  it "doesnt't find deleted users by userName or externalId" $ testFindNoDeletedUsers
  it "doesn't list users from other teams" $ testUserListFailsWithNotFoundIfOutsideTeam
  it "doesn't find users from other teams" $ testUserFindFailsWithNotFoundIfOutsideTeam

-- | Test that SCIM-provisioned team members are listed, and users that were not provisioned
-- via SCIM are not listed.
testListProvisionedUsers :: TestSpar ()
testListProvisionedUsers = do
  spar <- asks (^. teSpar)
  (tok, _) <- registerIdPAndScimToken
  listUsers_ (Just tok) Nothing spar !!! do
    const 400 === statusCode
    const (Just "tooMany") =~= responseBody

testFindProvisionedUser :: TestSpar ()
testFindProvisionedUser = do
  user <- randomScimUser
  (tok, (_, _, _)) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  users <- listUsers tok (Just (filterBy "userName" (Scim.User.userName user)))
  liftIO $ users `shouldBe` [storedUser]
  let Just externalId = Scim.User.externalId user
  users' <- listUsers tok (Just (filterBy "externalId" externalId))
  liftIO $ users' `shouldBe` [storedUser]

-- When explicitly filtering, we should be able to find non-SCIM-provisioned users
testFindNonProvisionedUser :: TestSpar ()
testFindNonProvisionedUser = do
  (_, teamid, idp, (_, privCreds)) <- registerTestIdPWithMeta
  member <- loginSsoUserFirstTime idp privCreds
  -- NOTE: once SCIM is enabled SSO Auto-provisioning is disabled
  tok <- registerScimToken teamid (Just (idp ^. SAML.idpId))
  handle'@(Handle handle) <- nextHandle
  runSpar $ Intra.setBrigUserHandle member handle'
  Just brigUser <- runSpar $ Intra.getBrigUser member
  liftIO $ userManagedBy brigUser `shouldBe` ManagedByWire
  users <- listUsers tok (Just (filterBy "userName" handle))
  liftIO $ (scimUserId <$> users) `shouldContain` [member]
  Just brigUser' <- runSpar $ Intra.getBrigUser member
  liftIO $ userManagedBy brigUser' `shouldBe` ManagedByScim
  -- a scim record should've been inserted too
  _ <- getUser tok member
  let Just ssoIdentity' = userIdentity >=> ssoIdentity $ brigUser'
  let Right externalId = Intra.toExternalId ssoIdentity'
  users' <- listUsers tok (Just (filterBy "externalId" externalId))
  liftIO $ (scimUserId <$> users') `shouldContain` [member]

-- | Test that deleted users are not listed.
testListNoDeletedUsers :: TestSpar ()
testListNoDeletedUsers = do
  env <- ask
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  let userid = scimUserId storedUser
  -- Delete the user
  -- TODO(fisx): use 'Util.Scim.deleteUser' instead of 'Util.Core.deleteUserOnBrig' (in fact,
  -- we should probably do both)
  call $ deleteUserOnBrig (env ^. teBrig) userid
  -- Get all users
  users <- listUsers tok (Just (filterForStoredUser storedUser))
  -- Check that the user is absent
  liftIO $ users `shouldSatisfy` all ((/= userid) . scimUserId)

testFindNoDeletedUsers :: TestSpar ()
testFindNoDeletedUsers = do
  env <- ask
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  let userid = scimUserId storedUser
  call $ deleteUserOnBrig (env ^. teBrig) userid
  let Just externalId = Scim.User.externalId user
  users' <- listUsers tok (Just (filterBy "externalId" externalId))
  liftIO $ users' `shouldSatisfy` all ((/= userid) . scimUserId)
  users'' <- listUsers tok (Just (filterBy "userName" (Scim.User.userName user)))
  liftIO $ users'' `shouldSatisfy` all ((/= userid) . scimUserId)

-- | Test that users are not listed if not in the team associated with the token.
testUserListFailsWithNotFoundIfOutsideTeam :: HasCallStack => TestSpar ()
testUserListFailsWithNotFoundIfOutsideTeam = do
  user <- randomScimUser
  (tokTeamA, _) <- registerIdPAndScimToken
  (tokTeamB, _) <- registerIdPAndScimToken
  storedUser <- createUser tokTeamA user
  let userid = scimUserId storedUser
  users <- listUsers tokTeamB (Just (filterForStoredUser storedUser))
  liftIO $ users `shouldSatisfy` all ((/= userid) . scimUserId)

testUserFindFailsWithNotFoundIfOutsideTeam :: TestSpar ()
testUserFindFailsWithNotFoundIfOutsideTeam = do
  user <- randomScimUser
  (tokTeamA, _) <- registerIdPAndScimToken
  (tokTeamB, _) <- registerIdPAndScimToken
  storedUser <- createUser tokTeamA user
  let userid = scimUserId storedUser
  users <- listUsers tokTeamB (Just (filterBy "userName" (Scim.User.userName user)))
  liftIO $ users `shouldSatisfy` all ((/= userid) . scimUserId)

----------------------------------------------------------------------------
-- Fetching a single user

-- | Tests for @GET \/Users\/:id@.
specGetUser :: SpecWith TestEnv
specGetUser = describe "GET /Users/:id" $ do
  it "finds a SCIM-provisioned user" testGetUser
  it "does not find a user invited old-school via team-settings" testGetNonScimInviteUser
  it "finds a user auto-provisioned via SAML and puts it under SCIM management" testGetNonScimSAMLUser
  it "finds a user that has no handle, and gives it a default handle before responding with it" testGetUserWithNoHandle
  it "doesn't find a deleted user" testGetNoDeletedUsers
  it "doesn't find users from other teams" testUserGetFailsWithNotFoundIfOutsideTeam

-- | Test that a SCIM-provisioned user is fetchable.
testGetUser :: TestSpar ()
testGetUser = do
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  -- Check that the SCIM-provisioned user can be fetched
  storedUser' <- getUser tok (scimUserId storedUser)
  liftIO $ storedUser' `shouldBe` storedUser

shouldBeManagedBy :: HasCallStack => UserId -> ManagedBy -> TestSpar ()
shouldBeManagedBy uid flag = do
  managedBy <- maybe (error "user not found") userManagedBy <$> runSpar (Intra.getBrigUser uid)
  liftIO $ managedBy `shouldBe` flag

-- | This is (roughly) the behavior on develop as well as on the branch where this test was
-- turned on.  TODO: find out if this is really what we want, or open an issue and reference
-- the issue here.
testGetNonScimSAMLUser :: TestSpar ()
testGetNonScimSAMLUser = do
  (_, tid, idp, (_, privcreds)) <- registerTestIdPWithMeta
  -- NOTE: once SCIM is enabled SSO Auto-provisioning is disabled, so we register the scim token later.

  uidSso <- loginSsoUserFirstTime idp privcreds
  tok <- registerScimToken tid (Just (idp ^. SAML.idpId))

  shouldBeManagedBy uidSso ManagedByWire
  _ <- getUser tok uidSso
  shouldBeManagedBy uidSso ManagedByScim

testGetNonScimInviteUser :: TestSpar ()
testGetNonScimInviteUser = do
  brig <- asks (^. teBrig)
  (tok, (owner, tid, _)) <- registerIdPAndScimToken

  uidNoSso <- userId <$> call (inviteAndRegisterUser brig owner tid)
  getUser_ (Just tok) uidNoSso brig !!! const 404 === statusCode

testGetUserWithNoHandle :: TestSpar ()
testGetUserWithNoHandle = do
  (_, tid, idp, (_, privcreds)) <- registerTestIdPWithMeta
  -- NOTE: once SCIM is enabled SSO Auto-provisioning is disabled, so we register the scim token later.
  uid <- loginSsoUserFirstTime idp privcreds
  tok <- registerScimToken tid (Just (idp ^. SAML.idpId))

  mhandle :: Maybe Handle <- maybe (error "user not found") userHandle <$> runSpar (Intra.getBrigUser uid)
  liftIO $ mhandle `shouldSatisfy` isNothing

  storedUser <- getUser tok uid
  liftIO $ (Scim.User.displayName . Scim.value . Scim.thing) storedUser `shouldSatisfy` isJust
  mhandle' :: Maybe Handle <- aFewTimes (maybe (error "user not found") userHandle <$> runSpar (Intra.getBrigUser uid)) isJust
  liftIO $ mhandle' `shouldSatisfy` isJust
  liftIO $ (fromHandle <$> mhandle') `shouldBe` (Just . Scim.User.userName . Scim.value . Scim.thing $ storedUser)

-- | Test that a deleted SCIM-provisioned user is not fetchable.
testGetNoDeletedUsers :: TestSpar ()
testGetNoDeletedUsers = do
  env <- ask
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  let userid = scimUserId storedUser
  -- Delete the user
  call $ deleteUserOnBrig (env ^. teBrig) userid
  -- Try to find the user
  getUser_ (Just tok) userid (env ^. teSpar)
    !!! const 404 === statusCode
  -- TODO(arianvp): What does this mean; @fisx ??
  pendingWith "TODO: delete via SCIM"

-- | Test that gets are not allowed if token is not for the user's team
testUserGetFailsWithNotFoundIfOutsideTeam :: TestSpar ()
testUserGetFailsWithNotFoundIfOutsideTeam = do
  env <- ask
  user <- randomScimUser
  (tokTeamA, _) <- registerIdPAndScimToken
  (tokTeamB, _) <- registerIdPAndScimToken
  storedUser <- createUser tokTeamA user
  let userid = scimUserId storedUser
  getUser_ (Just tokTeamB) userid (env ^. teSpar)
    !!! const 404 === statusCode

{- does not find a non-scim-provisioned user:

    env <- ask
    -- Check that the (non-SCIM-provisioned) team owner can be fetched and that the
    -- data from Brig matches
    brigUser <- fmap responseJsonUnsafe . call . get $
        ( (env ^. teBrig)
        . header "Z-User" (toByteString' (env^.teUserId))
        . path "/self"
        . expect2xx
        )
    scimStoredUser <- getUser (env^.teUserId)
    scimStoredUser `userShouldMatch` brigUser
-}

----------------------------------------------------------------------------
-- Updating users

-- | Tests for @PUT \/Users\/:id@.
specUpdateUser :: SpecWith TestEnv
specUpdateUser = describe "PUT /Users/:id" $ do
  it "requires a user ID" $ testUpdateRequiresUserId
  it "updates user attributes in scim_user" $ testScimSideIsUpdated
  it "works fine when neither name nor handle are changed" $ testUpdateSameHandle
  it "updates the 'SAML.UserRef' index in Spar" $ testUpdateUserRefIndex
  it "updates the matching Brig user" $ testBrigSideIsUpdated
  it
    "cannot update user to match another user's externalId"
    testUpdateToExistingExternalIdFails
  it "cannot remove display name" $ testCannotRemoveDisplayName
  context "user is from different team" $ do
    it "fails to update user with 404" testUserUpdateFailsWithNotFoundIfOutsideTeam
  context "scim_user has no entry with this id" $ do
    it "fails" $ pending
  it "does not update if nothing changed" $ testSameUpdateNoChange
  context "brig user is updated" $ do
    -- TODO(arianvp): This will be fixed by
    -- https://github.com/zinfra/backend-issues/issues/1006 The comment
    -- means: If we update a user on the brig side; then currently this is
    -- not reflected on the SCIM side. We can fix this by making brig
    -- actually the source of truth for SCIM SCIM then becomes a _view_;
    -- not a separate database.
    it "does NOT mirror this in the scim user" $
      pendingWith
        "this is arguably not great behavior, but i'm not sure \
        \we want to implement synchronisation from brig to spar?"
    it "updates to scim user will overwrite these updates" $
      pendingWith "that's probably what we want?"

-- | Tests that you can't unset your display name
testCannotRemoveDisplayName :: TestSpar ()
testCannotRemoveDisplayName = do
  -- NOTE: This behaviour is in violation of SCIM.
  -- We either:
  --  - Treat Null and omission the same; by always removing
  --  - Or default on omissison, delete on null
  --  We have to choose between the two behaviours; in order
  --  to be a valid SCIM API. we now do an akward mixture of both
  --  However; I don't think this is currently blocking us on Azure.
  --  We should however fix this behaviour in the future TODO(arianvp)
  pendingWith
    "We default to the externalId when displayName is removed. lets keep that for now"

{-
env <- ask
user <- randomScimUser
(tok, _) <- registerIdPAndScimToken
storedUser <- createUser tok user
let userid = scimUserId storedUser
let user' = user { Scim.User.displayName = Nothing }
updateUser_ (Just tok) (Just userid) user'  (env ^. teSpar) !!! const 409 === statusCode
-}

-- | Test that when you're not changing any fields, then that update should not
-- change anything (Including version field)
testSameUpdateNoChange :: TestSpar ()
testSameUpdateNoChange = do
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  let userid = scimUserId storedUser
  storedUser' <- updateUser tok userid user
  liftIO $ storedUser `shouldBe` storedUser'

-- | Test that @PUT /Users@ returns 4xx when called without the @:id@ part.
testUpdateRequiresUserId :: TestSpar ()
testUpdateRequiresUserId = do
  env <- ask
  user <- randomScimUser
  (tok, _) <- registerIdPAndScimToken
  updateUser_ (Just tok) Nothing user (env ^. teSpar)
    !!! assertTrue_ (inRange (400, 499) . statusCode)

-- | Test that updates are not allowed if token is not for the user's team
testUserUpdateFailsWithNotFoundIfOutsideTeam :: TestSpar ()
testUserUpdateFailsWithNotFoundIfOutsideTeam = do
  env <- ask
  -- Create a user via SCIM
  user <- randomScimUser
  (tokTeamA, _) <- registerIdPAndScimToken
  (tokTeamB, _) <- registerIdPAndScimToken
  storedUser <- createUser tokTeamA user
  let userid = scimUserId storedUser
  -- Overwrite the user with another randomly-generated user
  user' <- randomScimUser
  updateUser_ (Just tokTeamB) (Just userid) user' (env ^. teSpar)
    !!! const 404 === statusCode

-- | Test that @PUT@-ting the user and then @GET@-ting it returns the right thing.
testScimSideIsUpdated :: TestSpar ()
testScimSideIsUpdated = do
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, (_, _, idp)) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  let userid = scimUserId storedUser
  -- Overwrite the user with another randomly-generated user
  user' <- randomScimUser
  updatedUser <- updateUser tok userid user'
  -- Get the updated user and check that it matches the user returned by
  -- 'updateUser'
  storedUser' <- getUser tok userid
  liftIO $ updatedUser `shouldBe` storedUser'
  -- Check that the updated user also matches the data that we sent with
  -- 'updateUser'
  richInfoLimit <- asks (Spar.Types.richInfoLimit . view teOpts)
  liftIO $ do
    Right (Scim.value (Scim.thing storedUser')) `shouldBe` whatSparReturnsFor idp richInfoLimit user'
    Scim.id (Scim.thing storedUser') `shouldBe` Scim.id (Scim.thing storedUser)
    let meta = Scim.meta storedUser
        meta' = Scim.meta storedUser'
    Scim.version meta `shouldNotBe` Scim.version meta'
    Scim.resourceType meta `shouldBe` Scim.resourceType meta'
    Scim.created meta `shouldBe` Scim.created meta'
    Scim.location meta `shouldBe` Scim.location meta'

-- | Test that updating a user with the externalId of another user fails
testUpdateToExistingExternalIdFails :: TestSpar ()
testUpdateToExistingExternalIdFails = do
  -- Create a user via SCIM
  (tok, _) <- registerIdPAndScimToken
  user <- randomScimUser
  _ <- createUser tok user
  newUser <- randomScimUser
  storedNewUser <- createUser tok newUser
  let userExternalId = Scim.User.externalId user
  -- Ensure we're actually generating an external ID; we may stop doing this in the future
  liftIO $ userExternalId `shouldSatisfy` isJust
  -- Try to update the new user's external ID to be the same as 'user's.
  let updatedNewUser = newUser {Scim.User.externalId = userExternalId}
  env <- ask
  -- Should fail with 409 to denote that the given externalId is in use by a
  -- different user.
  updateUser_ (Just tok) (Just $ scimUserId storedNewUser) updatedNewUser (env ^. teSpar)
    !!! const 409 === statusCode

-- | Test that updating still works when name and handle are not changed.
--
-- This test is needed because if @PUT \/Users@ is implemented in such a way that it /always/
-- tries to set the name and handle, it might fail because the handle is "already claimed".
testUpdateSameHandle :: TestSpar ()
testUpdateSameHandle = do
  -- Create a user via SCIM
  user <- randomScimUser
  (tok, (_, _, idp)) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  let userid = scimUserId storedUser
  -- Overwrite the user with another randomly-generated user who has the same name and
  -- handle
  user' <-
    randomScimUser <&> \u ->
      u
        { Scim.User.userName = Scim.User.userName user,
          Scim.User.displayName = Scim.User.displayName user
        }
  updatedUser <- updateUser tok userid user'
  -- Get the updated user and check that it matches the user returned by 'updateUser'
  storedUser' <- getUser tok userid
  liftIO $ updatedUser `shouldBe` storedUser'
  -- Check that the updated user also matches the data that we sent with 'updateUser'
  richInfoLimit <- asks (Spar.Types.richInfoLimit . view teOpts)
  liftIO $ do
    Right (Scim.value (Scim.thing storedUser')) `shouldBe` whatSparReturnsFor idp richInfoLimit user'
    Scim.id (Scim.thing storedUser') `shouldBe` Scim.id (Scim.thing storedUser)
    let meta = Scim.meta storedUser
        meta' = Scim.meta storedUser'
    Scim.version meta `shouldNotBe` Scim.version meta'
    Scim.resourceType meta `shouldBe` Scim.resourceType meta'
    Scim.created meta `shouldBe` Scim.created meta'
    Scim.location meta `shouldBe` Scim.location meta'

-- | Test that when a user's 'UserRef' is updated, the relevant index is also updated and Spar
-- can find the user by the 'UserRef'.
testUpdateUserRefIndex :: TestSpar ()
testUpdateUserRefIndex = do
  (tok, (_, _, idp)) <- registerIdPAndScimToken
  let checkUpdateUserRef :: Bool -> TestSpar ()
      checkUpdateUserRef changeUserRef = do
        -- Create a user via SCIM
        user <- randomScimUser
        storedUser <- createUser tok user
        let userid = scimUserId storedUser
        uref <- either (error . show) pure $ mkUserRef idp (Scim.User.externalId user)
        -- Overwrite the user with another randomly-generated user
        user' <-
          let upd u =
                if changeUserRef
                  then u
                  else u {Scim.User.externalId = Scim.User.externalId user}
           in randomScimUser <&> upd
        _ <- updateUser tok userid user'
        uref' <- either (error . show) pure $ mkUserRef idp (Scim.User.externalId user')
        muserid <- runSparCass $ Data.getSAMLUser uref
        muserid' <- runSparCass $ Data.getSAMLUser uref'
        liftIO $ do
          (changeUserRef, muserid)
            `shouldBe` (changeUserRef, if changeUserRef then Nothing else Just userid)
          (changeUserRef, muserid')
            `shouldBe` (changeUserRef, Just userid)
  checkUpdateUserRef True
  checkUpdateUserRef False

-- | Test that when the user is updated via SCIM, the data in Brig is also updated.
testBrigSideIsUpdated :: TestSpar ()
testBrigSideIsUpdated = do
  user <- randomScimUser
  (tok, (_, _, idp)) <- registerIdPAndScimToken
  storedUser <- createUser tok user
  user' <- randomScimUser
  let userid = scimUserId storedUser
  _ <- updateUser tok userid user'
  validScimUser <-
    either (error . show) pure $
      validateScimUser' idp 999999 user'
  brigUser <- maybe (error "no brig user") pure =<< runSpar (Intra.getBrigUser userid)
  brigUser `userShouldMatch` validScimUser

----------------------------------------------------------------------------
-- Patching users
specPatchUser :: SpecWith TestEnv
specPatchUser = do
  -- Context: PATCH is implemented in the hscim library as a getUser followed
  -- by a series of transformation on the User object, and then a putUser. The
  -- correctness of the series of transformations is tested in the hscim test
  -- suite; and is independent of spar code.  GET and PUT are both already
  -- tested in the spar code; so here we just simply have a few smoke tests We
  -- also describe the current limitations. (We only support three fields so
  -- far)
  describe "PATCH /Users/:id" $ do
    let replaceAttrib name value =
          PatchOp.Operation
            PatchOp.Replace
            (Just (PatchOp.NormalPath (Filter.topLevelAttrPath name)))
            (Just (toJSON value))
    let removeAttrib name =
          PatchOp.Operation
            PatchOp.Remove
            (Just (PatchOp.NormalPath (Filter.topLevelAttrPath name)))
            Nothing
    it "doing nothing doesn't change the user" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      storedUser' <- patchUser tok userid (PatchOp.PatchOp [])
      liftIO $ storedUser `shouldBe` storedUser'
    it "can update userName" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      user' <- randomScimUser
      let userName = Scim.User.userName user'
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      storedUser' <-
        patchUser tok userid $
          PatchOp.PatchOp
            [replaceAttrib "userName" userName]
      let user'' = Scim.value (Scim.thing storedUser')
      liftIO $ Scim.User.userName user'' `shouldBe` userName
    it "can't update to someone else's userName" $ do
      env <- ask
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      user' <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      _ <- createUser tok user'
      let patchOp = PatchOp.PatchOp [replaceAttrib "userName" (Scim.User.userName user')]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar) !!! const 409 === statusCode
    it "can't update to someone else's externalId" $ do
      env <- ask
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      user' <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      _ <- createUser tok user'
      let patchOp = PatchOp.PatchOp [replaceAttrib "externalId" (Scim.User.externalId user')]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar) !!! const 409 === statusCode
    it "can't update a non-existing user" $ do
      env <- ask
      (tok, _) <- registerIdPAndScimToken
      userid <- liftIO $ randomId
      let patchOp = PatchOp.PatchOp [replaceAttrib "externalId" ("blah" :: Text)]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar) !!! const 404 === statusCode
    it "can update displayName" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      user' <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      let displayName = Scim.User.displayName user'
      storedUser' <-
        patchUser tok userid $
          PatchOp.PatchOp
            [replaceAttrib "displayName" displayName]
      let user'' = Scim.value (Scim.thing storedUser')
      liftIO $ Scim.User.displayName user'' `shouldBe` displayName
    it "can update externalId" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      user' <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      let externalId = Scim.User.externalId user'
      storedUser' <-
        patchUser tok userid $
          PatchOp.PatchOp
            [replaceAttrib "externalId" externalId]
      let user'' = Scim.value . Scim.thing $ storedUser'
      liftIO $ Scim.User.externalId user'' `shouldBe` externalId
    it "replacing every supported atttribute at once works" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      user' <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      let externalId = Scim.User.externalId user'
      let userName = Scim.User.userName user'
      let displayName = Scim.User.displayName user'
      storedUser' <-
        patchUser tok userid $
          PatchOp.PatchOp
            [ replaceAttrib "externalId" externalId,
              replaceAttrib "userName" userName,
              replaceAttrib "displayName" displayName
            ]
      let user'' = Scim.value . Scim.thing $ storedUser'
      liftIO $ Scim.User.externalId user'' `shouldBe` externalId
      liftIO $ Scim.User.userName user'' `shouldBe` userName
      liftIO $ Scim.User.displayName user'' `shouldBe` displayName
    it "other valid attributes that we do not explicit support throw an error" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      env <- ask
      let patchOp = PatchOp.PatchOp [replaceAttrib "emails" ("hello" :: Text)]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar)
        !!! const 400 === statusCode
    it "invalid attributes are quietly ignored for now" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      env <- ask
      let patchOp = PatchOp.PatchOp [replaceAttrib "totallyBogus" ("hello" :: Text)]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar)
        !!! const 400 === statusCode
    -- NOTE: Remove at the moment actually never works! As all the fields
    -- we support are required in our book
    it "userName cannot be removed according to scim" $ do
      env <- ask
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      let patchOp = PatchOp.PatchOp [removeAttrib "userName"]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar) !!! const 400 === statusCode
    it "displayName cannot be removed in spar (though possible in scim). Diplayname is required in Wire" $ do
      pendingWith
        "We default to the externalId when displayName is removed. lets keep that for now"
    {-env <- ask
    (tok, _) <- registerIdPAndScimToken
    user <- randomScimUser
    storedUser <- createUser tok user
    let userid = scimUserId storedUser
    let patchOp = PatchOp.PatchOp [ removeAttrib "displayName" ]
    patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar) !!! const 400 === statusCode -}
    it "externalId cannot be removed in spar (though possible in scim)" $ do
      env <- ask
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let userid = scimUserId storedUser
      let patchOp = PatchOp.PatchOp [removeAttrib "externalId"]
      patchUser_ (Just tok) (Just userid) patchOp (env ^. teSpar) !!! const 400 === statusCode

----------------------------------------------------------------------------
-- Deleting users

specDeleteUser :: SpecWith TestEnv
specDeleteUser = do
  describe "DELETE /Users" $ do
    it "responds with 405 (just making sure...)" $ do
      env <- ask
      (tok, _) <- registerIdPAndScimToken
      deleteUser_ (Just tok) Nothing (env ^. teSpar)
        !!! const 405 === statusCode
  describe "DELETE /Users/:id" $ do
    it "should delete user from brig, spar.scim_user, spar.user" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let uid :: UserId = scimUserId storedUser
      uref :: SAML.UserRef <- do
        usr <- runSpar $ Intra.getBrigUser uid
        maybe (error "no UserRef from brig") pure $ urefFromBrig =<< usr
      spar <- view teSpar
      deleteUser_ (Just tok) (Just uid) spar
        !!! const 204 === statusCode
      brigUser :: Maybe User <-
        aFewTimes (runSpar $ Intra.getBrigUser uid) isNothing
      samlUser :: Maybe UserId <-
        aFewTimes (getUserIdViaRef' uref) isNothing
      scimUser <-
        aFewTimes (runSparCass $ Data.readScimUserTimes uid) isNothing
      liftIO $
        (brigUser, samlUser, scimUser)
          `shouldBe` (Nothing, Nothing, Nothing)
    it "should respond with 204 on first deletion, then 404" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let uid = scimUserId storedUser
      spar <- view teSpar
      -- Expect first call to succeed
      deleteUser_ (Just tok) (Just uid) spar
        !!! const 204 === statusCode
      -- Subsequent calls will return 404 eventually
      aFewTimes (deleteUser_ (Just tok) (Just uid) spar) ((== 404) . statusCode)
        !!! const 404 === statusCode
    it "should free externalId and everything else in the scim user for re-use" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let uid :: UserId = scimUserId storedUser
      spar <- view teSpar
      deleteUser_ (Just tok) (Just uid) spar
        !!! const 204 === statusCode
      aFewTimes (createUser_ (Just tok) user spar) ((== 201) . statusCode)
        !!! const 201 === statusCode
    -- FUTUREWORK: hscim has the the following test.  we should probably go through all
    -- `delete` tests and see if they can move to hscim or are already included there.

    it "should return 401 if we don't provide a token" $ do
      user <- randomScimUser
      (tok, _) <- registerIdPAndScimToken
      storedUser <- createUser tok user
      spar <- view teSpar
      let uid = scimUserId storedUser
      deleteUser_ Nothing (Just uid) spar
        !!! const 401 === statusCode
    it "should return 404 if we provide a token for a different team" $ do
      (tok, _) <- registerIdPAndScimToken
      user <- randomScimUser
      storedUser <- createUser tok user
      let uid = scimUserId storedUser
      (invalidTok, _) <- registerIdPAndScimToken
      spar <- view teSpar
      deleteUser_ (Just invalidTok) (Just uid) spar
        !!! const 404 === statusCode
    it "getUser should return 404 after deleteUser" $ do
      user <- randomScimUser
      (tok, _) <- registerIdPAndScimToken
      storedUser <- createUser tok user
      spar <- view teSpar
      let uid = scimUserId storedUser
      deleteUser_ (Just tok) (Just uid) spar
        !!! const 204 === statusCode
      aFewTimes (getUser_ (Just tok) uid spar) ((== 404) . statusCode)
        !!! const 404 === statusCode
    it "whether implemented or not, does *NOT EVER* respond with 5xx!" $ do
      env <- ask
      user <- randomScimUser
      (tok, _) <- registerIdPAndScimToken
      storedUser <- createUser tok user
      deleteUser_ (Just tok) (Just $ scimUserId storedUser) (env ^. teSpar)
        !!! assertTrue_ (inRange (200, 499) . statusCode)

-- | Azure sends a request for an unknown user to test out whether your API is online However;
-- it sends a userName that is not a valid wire handle. So we should treat 'invalid' as 'not
-- found'.
specAzureQuirks :: SpecWith TestEnv
specAzureQuirks = do
  describe "Assert that we implement all azure quirks" $ do
    it "GET /Users?filter=randomField eq <invalid value> should return empty list; not error out" $ do
      (tok, (_, _, _)) <- registerIdPAndScimToken
      users <- listUsers tok (Just (filterBy "userName" "f52dcb88-9fa1-4ec7-984f-7bc2d4046a9c"))
      liftIO $ users `shouldBe` []
      users' <- listUsers tok (Just (filterBy "externalId" "f52dcb88-9fa1-4ec7-984f-7bc2d4046a9c"))
      liftIO $ users' `shouldBe` []

----------------------------------------------------------------------------
-- Email validation of SAML users (depending on team flag)

specEmailValidation :: SpecWith TestEnv
specEmailValidation = do
  describe "email validation" $ do
    let enableSamlEmailValidation :: HasCallStack => TeamId -> TestSpar ()
        enableSamlEmailValidation tid = do
          galley <- asks (^. teGalley)
          let req = put $ galley . paths p . json (Feature.TeamFeatureStatus Feature.TeamFeatureEnabled)
              p = ["/i/teams", toByteString' tid, "features", "validate-saml-emails"]
          call req !!! const 204 === statusCode
        --
        assertEmail :: HasCallStack => UserId -> Maybe Email -> TestSpar ()
        assertEmail uid expectedEmail = do
          brig <- asks (^. teBrig)
          let req = get (brig . path "/self" . zUser uid)
          call req !!! do
            const 200 === statusCode
            const expectedEmail === (userEmail <=< responseJsonMaybe)
        --
        eventually :: HasCallStack => TestSpar a -> TestSpar a
        eventually = recovering (limitRetries 3 <> exponentialBackoff 100000) [] . const
        --
        setup :: HasCallStack => Bool -> TestSpar (UserId, Email)
        setup enabled = do
          (tok, (_ownerid, teamid, idp)) <- registerIdPAndScimToken
          when enabled $ enableSamlEmailValidation teamid
          (user, email) <- randomScimUserWithEmail
          scimStoredUser <- createUser tok user
          let Right uref = mkUserRef idp . Scim.User.externalId . Scim.value . Scim.thing $ scimStoredUser
          uid <- getUserIdViaRef uref
          brig <- asks (^. teBrig)
          call $ activateEmail brig email
          pure (uid, email)
        --
        -- copied from brig integration tests.
        activateEmail ::
          HasCallStack =>
          BrigReq ->
          Email ->
          (MonadIO m, MonadCatch m, MonadHttp m) => m ()
        activateEmail brig email = do
          act <- getActivationCode brig (Left email)
          case act of
            Nothing -> pure () -- missing activation key/code; this happens if the feature is
            -- disabled (second test case below)
            Just kc ->
              activate brig kc !!! do
                const 200 === statusCode
                const (Just False) === fmap Activation.activatedFirst . responseJsonMaybe
        --
        -- copied from brig integration tests.
        getActivationCode ::
          HasCallStack =>
          BrigReq ->
          Either Email Phone ->
          (MonadIO m, MonadCatch m, MonadHttp m) => m (Maybe (Activation.ActivationKey, Activation.ActivationCode))
        getActivationCode brig ep = do
          let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
          r <- get $ brig . path "/i/users/activation-code" . qry
          let lbs = fromMaybe "" $ responseBody r
          let akey = Activation.ActivationKey . Ascii.unsafeFromText <$> (lbs ^? key "key" . _String)
          let acode = Activation.ActivationCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
          return $ (,) <$> akey <*> acode
        --
        -- copied from brig integration tests.
        activate ::
          HasCallStack =>
          BrigReq ->
          (Activation.ActivationKey, Activation.ActivationCode) ->
          (MonadIO m, MonadCatch m, MonadHttp m) => m ResponseLBS
        activate brig (k, c) =
          get $
            brig
              . path "activate"
              . queryItem "key" (toByteString' k)
              . queryItem "code" (toByteString' c)

    context "enabled in team" . it "gives user email" $ do
      (uid, email) <- setup True
      eventually $ assertEmail uid (Just email)

    context "not enabled in team" . it "does not give user email" $ do
      (uid, _) <- setup False
      eventually $ assertEmail uid Nothing
