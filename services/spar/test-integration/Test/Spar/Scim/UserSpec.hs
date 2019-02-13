{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for the branch of SCIM API that deals with users (@\/scim\/v2\/Users@).
module Test.Spar.Scim.UserSpec (spec) where

import Imports
import Bilge
import Bilge.Assert
import Brig.Types.User as Brig
import Control.Lens
import Data.ByteString.Conversion
import Data.Ix (inRange)
import Spar.Scim
import Util

import qualified Spar.Data                        as Data
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.User             as Scim.User


-- | Tests for @\/scim\/v2\/Users@.
spec :: SpecWith TestEnv
spec = do
    specCreateUser
    specListUsers
    specGetUser
    specUpdateUser

    describe "DELETE /Users" $ do
        it "responds with 404 (just making sure...)" $ pending
    describe "DELETE /Users/:id" $ do
        it "sets the 'deleted' flag in brig, and does nothing otherwise." $
            pendingWith "really?  how do we destroy the data then, and when?"
    describe "CRUD operations maintain invariants in mapScimToBrig, mapBrigToScim." $ do
        it "..." $ do
            pendingWith "this is a job for quickcheck-state-machine"
    describe "validateScimUser'" $ do
        it "works" $ do
            pendingWith "write a list of unit tests here that make the mapping explicit, exhaustive, and easy to read."

----------------------------------------------------------------------------
-- User creation

-- | Tests for @POST /Users@.
specCreateUser :: SpecWith TestEnv
specCreateUser = describe "POST /Users" $ do
    it "creates a user in an existing team" $ testCreateUser
    it "requires externalId to be present" $ testExternalIdIsRequired
    it "rejects invalid handle" $ testCreateRejectsInvalidHandle
    it "gives created user a valid 'SAML.UserRef' for SSO" $ pending
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
    brigUser :: User <- fmap decodeBody' . call . get $
        ( (env ^. teBrig)
        . header "Z-User" (toByteString' userid)
        . path "/self"
        . expect2xx
        )
    brigUser `userShouldMatch` scimStoredUser

-- | Test that @externalId@ (for SSO login) is required when creating a user.
testExternalIdIsRequired :: TestSpar ()
testExternalIdIsRequired = do
    env <- ask
    -- Create a user with a missing @externalId@ and check that it fails
    user <- randomScimUser
    let user' = user { Scim.User.externalId = Nothing }
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
    createUser_ (Just tok) (user{Scim.User.userName="#invalid name"}) (env ^. teSpar)
      !!! const 400 === statusCode

----------------------------------------------------------------------------
-- Listing users

-- | Tests for @GET /Users@.
specListUsers :: SpecWith TestEnv
specListUsers = describe "GET /Users" $ do
    it "lists all users in a team" $ testListAllUsers
    it "finds a SCIM-provisioned user by username" $ pending
    it "finds a non-SCIM-provisioned user by username" $ pending
    it "doesn't list deleted users" $ testListNoDeletedUsers

-- | Test that SCIM-provisioned team members are listed, and users that were not provisioned
-- via SCIM are not listed.
testListAllUsers :: TestSpar ()
testListAllUsers = do
    -- Create a user via SCIM
    user <- randomScimUser
    (tok, (owner, _, _)) <- registerIdPAndScimToken
    storedUser <- createUser tok user
    -- Get all users via SCIM
    users <- listUsers tok Nothing
    -- Check that the SCIM user is present
    liftIO $ users `shouldContain` [storedUser]
    -- Check that the (non-SCIM-provisioned) team owner is NOT present
    liftIO $ (scimUserId <$> users) `shouldNotContain` [owner]

-- | Test that deleted users are not listed.
testListNoDeletedUsers :: TestSpar ()
testListNoDeletedUsers = do
    env <- ask
    -- Create a user via SCIM
    user <- randomScimUser
    (tok, _) <- registerIdPAndScimToken
    storedUser <- createUser tok user
    let userid = scimUserId storedUser
    -- Delete the user (TODO: do it via SCIM)
    call $ deleteUser (env ^. teBrig) userid
    -- Get all users
    users <- listUsers tok Nothing
    -- Check that the user is absent
    liftIO $ users `shouldSatisfy` all ((/= userid) . scimUserId)

----------------------------------------------------------------------------
-- Fetching a single user

-- | Tests for @GET \/Users\/:id@.
specGetUser :: SpecWith TestEnv
specGetUser = describe "GET /Users/:id" $ do
    it "finds a SCIM-provisioned user" $ testGetUser
    it "does not find a non-SCIM-provisioned user" $ do
        pendingWith "TODO: fails because the user has no handle (UPDATE: \
                    \it *should* fail, too, just need to make sure it's \
                    \for the right reasons.)"
    it "doesn't find a user that's not part of the team" $ do
        pending
        -- create another team and another user in it
        -- check that this user can not be found in the "wrong" team
    it "doesn't find a deleted user" $ testGetNoDeletedUsers

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
    call $ deleteUser (env ^. teBrig) userid
    -- Try to find the user
    getUser_ (Just tok) userid (env ^. teSpar)
        !!! const 404 === statusCode
    pendingWith "TODO: delete via SCIM"

{- does not find a non-scim-provisioned user:

    env <- ask
    -- Check that the (non-SCIM-provisioned) team owner can be fetched and that the
    -- data from Brig matches
    brigUser <- fmap decodeBody' . call . get $
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
    context "user is from different team" $ do
        it "fails to update user with 404" testUserUpdateFailsWithNotFoundIfOutsideTeam
    context "scim_user has no entry with this id" $ do
        it "fails" $ pending
    context "brig user is updated" $ do
        it "does NOT mirror this in the scim user" $
            pendingWith "this is arguably not great behavior, but i'm not sure \
                        \we want to implement synchronisation from brig to spar?"
        it "updates to scim user will overwrite these updates" $
            pendingWith "that's probably what we want?"

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
    (tok, _) <- registerIdPAndScimToken
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
    liftIO $ do
        Scim.value (Scim.thing storedUser') `shouldBe` user'
        Scim.id (Scim.thing storedUser') `shouldBe` Scim.id (Scim.thing storedUser)
        let meta  = Scim.meta storedUser
            meta' = Scim.meta storedUser'
        Scim.version meta `shouldNotBe` Scim.version meta'
        Scim.resourceType meta `shouldBe` Scim.resourceType meta'
        Scim.created meta `shouldBe` Scim.created meta'
        Scim.location meta `shouldBe` Scim.location meta'

-- | Test that updating still works when name and handle are not changed.
--
-- This test is needed because if @PUT \/Users@ is implemented in such a way that it /always/
-- tries to set the name and handle, it might fail because the handle is "already claimed".
testUpdateSameHandle :: TestSpar ()
testUpdateSameHandle = do
    -- Create a user via SCIM
    user <- randomScimUser
    (tok, _) <- registerIdPAndScimToken
    storedUser <- createUser tok user
    let userid = scimUserId storedUser
    -- Overwrite the user with another randomly-generated user who has the same name and
    -- handle
    user' <- randomScimUser <&> \u ->
        u { Scim.User.userName = Scim.User.userName user
          , Scim.User.displayName = Scim.User.displayName user
          }
    updatedUser <- updateUser tok userid user'
    -- Get the updated user and check that it matches the user returned by 'updateUser'
    storedUser' <- getUser tok userid
    liftIO $ updatedUser `shouldBe` storedUser'
    -- Check that the updated user also matches the data that we sent with 'updateUser'
    liftIO $ do
        Scim.value (Scim.thing storedUser') `shouldBe` user'
        Scim.id (Scim.thing storedUser') `shouldBe` Scim.id (Scim.thing storedUser)
        let meta  = Scim.meta storedUser
            meta' = Scim.meta storedUser'
        Scim.version meta `shouldNotBe` Scim.version meta'
        Scim.resourceType meta `shouldBe` Scim.resourceType meta'
        Scim.created meta `shouldBe` Scim.created meta'
        Scim.location meta `shouldBe` Scim.location meta'

-- | Test that when a user's 'UserRef' is updated, the relevant index is also updated and Spar
-- can find the user by the 'UserRef'.
testUpdateUserRefIndex :: TestSpar ()
testUpdateUserRefIndex = do
    -- Create a user via SCIM
    user <- randomScimUser
    (tok, (_, _, idp)) <- registerIdPAndScimToken
    storedUser <- createUser tok user
    let userid = scimUserId storedUser
    -- Overwrite the user with another randomly-generated user
    user' <- randomScimUser
    _ <- updateUser tok userid user'
    vuser' <- either (error . show) pure $ validateScimUser' (Just idp) user'
    muserid' <- runSparCass $ Data.getUser (vuser' ^. vsuSAMLUserRef)
    liftIO $ do
        muserid' `shouldBe` Just userid

-- | Test that when the user is updated via SCIM, the data in Brig is also updated.
testBrigSideIsUpdated :: TestSpar ()
testBrigSideIsUpdated = do
    user <- randomScimUser
    (tok, (_, _, idp)) <- registerIdPAndScimToken
    storedUser <- createUser tok user
    user' <- randomScimUser
    let userid = scimUserId storedUser
    _ <- updateUser tok userid user'
    validScimUser <- either (error . show) pure $ validateScimUser' (Just idp) user'
    brigUser      <- maybe (error "no brig user") pure =<< getSelf userid
    brigUser `userShouldMatch` validScimUser
