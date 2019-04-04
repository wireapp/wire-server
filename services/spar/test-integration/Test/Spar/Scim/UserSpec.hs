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
import Data.Id (UserId)
import Data.Ix (inRange)
import Spar.Scim
import Util

import qualified SAML2.WebSSO.Types               as SAML
import qualified Spar.Data                        as Data
import qualified Spar.Intra.Brig                  as Intra
import qualified Web.Scim.Class.User              as ScimC.User
import qualified Web.Scim.Class.User              as Scim.UserC
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
    specDeleteUser

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
    it "rejects occupied handle" $ testCreateRejectsTakenHandle
    it "rejects occupied externalId" $ testCreateRejectsTakenExternalId
    it "allows an occupied externalId when the IdP is different" $
        testCreateSameExternalIds
    it "provides a correct location in the 'meta' field" $ testLocation
    it "handles rich info correctly (this also tests put, get)" $ testRichInfo
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
    createUser_ (Just tokTeamA) (user2 { Scim.User.userName = Scim.User.userName user1 }) (env ^. teSpar)
        !!! const 409 === statusCode

    -- Try to create different user with same handle in different team.
    createUser_ (Just tokTeamB) (user3 { Scim.User.userName = Scim.User.userName user1 }) (env ^. teSpar)
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
    createUser_ (Just tok) (user2 { Scim.User.externalId = Scim.User.externalId user1 }) (env ^. teSpar)
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
    _ <- createUser tokTeamB (user2 { Scim.User.externalId = Scim.User.externalId user1 })
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
    req <- parseRequest (show (Scim.unURI location))
               <&> scimAuth (Just tok) . acceptScim
    r <- call (get (const req)) <!! const 200 === statusCode
    liftIO $ decodeBody' r `shouldBe` scimStoredUser

testRichInfo :: TestSpar ()
testRichInfo = do
    brig <- asks (view teBrig)

    -- set things up
    let richInfo  = RichInfo [RichField "Platforms" "OpenBSD; Plan9"]
        richInfo' = RichInfo [RichField "Platforms" "Windows10"]
    (user, _)  <- randomScimUserWithSubjectAndRichInfo richInfo
    (user', _) <- randomScimUserWithSubjectAndRichInfo richInfo'
    (tok, (owner, _, _)) <- registerIdPAndScimToken

    let -- validate response
        checkStoredUser
            :: HasCallStack
            => Scim.UserC.StoredUser ScimUserExtra -> RichInfo -> TestSpar ()
        checkStoredUser storedUser rinf = liftIO $ do
            (Scim.User.extra . Scim.value . Scim.thing) storedUser
                `shouldBe` (ScimUserExtra rinf)

        -- validate server state after the fact
        probeUser
            :: HasCallStack
            => UserId -> RichInfo -> TestSpar ()
        probeUser uid rinf = do
            -- get scim user yields correct rich info.
            scimStoredUser' <- getUser tok uid
            checkStoredUser scimStoredUser' rinf

            -- get rich info end-point on brig yields correct rich info.
            resp <- call $ get ( brig
                               . paths ["users", toByteString' uid, "rich-info"]
                               . zUser owner
                               )
            liftIO $ do
                statusCode resp `shouldBe` 200
                decodeBody resp `shouldBe` Right rinf

    -- post response contains correct rich info.
    scimStoredUser <- createUser tok user
    checkStoredUser scimStoredUser richInfo

    -- post updates the backend as expected.
    probeUser (scimUserId scimStoredUser) richInfo

    -- put response contains correct rich info.
    scimStoredUser' <- updateUser tok (scimUserId scimStoredUser) user'
    checkStoredUser scimStoredUser' richInfo'

    -- post updates the backend as expected.
    liftIO $ scimUserId scimStoredUser' `shouldBe` scimUserId scimStoredUser
    probeUser (scimUserId scimStoredUser) richInfo'

----------------------------------------------------------------------------
-- Listing users

-- | Tests for @GET /Users@.
specListUsers :: SpecWith TestEnv
specListUsers = describe "GET /Users" $ do
    it "lists all users in a team" $ testListAllUsers
    it "finds a SCIM-provisioned user by username" $ pending
    it "finds a non-SCIM-provisioned user by username" $ pending
    it "doesn't list deleted users" $ testListNoDeletedUsers
    it "doesn't find users from other teams" $ testUserListFailsWithNotFoundIfOutsideTeam

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
    call $ deleteUserOnBrig (env ^. teBrig) userid
    -- Get all users
    users <- listUsers tok Nothing
    -- Check that the user is absent
    liftIO $ users `shouldSatisfy` all ((/= userid) . scimUserId)

-- | Test that users are not listed if not in the team associated with the token.
testUserListFailsWithNotFoundIfOutsideTeam :: TestSpar ()
testUserListFailsWithNotFoundIfOutsideTeam = do
    user <- randomScimUser
    (tokTeamA, _) <- registerIdPAndScimToken
    (tokTeamB, _) <- registerIdPAndScimToken
    storedUser <- createUser tokTeamA user
    let userid = scimUserId storedUser
    users <- listUsers tokTeamB Nothing
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
    it "doesn't find users from other teams" $ testUserGetFailsWithNotFoundIfOutsideTeam

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
    call $ deleteUserOnBrig (env ^. teBrig) userid
    -- Try to find the user
    getUser_ (Just tok) userid (env ^. teSpar)
        !!! const 404 === statusCode
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
    it "cannot update user to match another user's externalId"
        testUpdateToExistingExternalIdFails
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
    let updatedNewUser = newUser{Scim.User.externalId = userExternalId}

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
    vuser' <- either (error . show) pure $
        validateScimUser' idp 999999 user'  -- 999999 = some big number
    muserid' <- runSparCass $ Data.getSAMLUser (vuser' ^. vsuSAMLUserRef)
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
    validScimUser <- either (error . show) pure $
        validateScimUser' idp 999999 user'
    brigUser      <- maybe (error "no brig user") pure =<< runSpar (Intra.getBrigUser userid)
    brigUser `userShouldMatch` validScimUser

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

            brigUser :: Maybe User
              <- eventually (runSpar $ Intra.getBrigUser uid) isNothing
            samlUser :: Maybe UserId
              <- eventually (getUserIdViaRef' uref) isNothing
            scimUser :: Maybe (ScimC.User.StoredUser ScimUserExtra)
              <- eventually (getScimUser uid) isNothing

            liftIO $ (brigUser, samlUser, scimUser)
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
            eventually (deleteUser_ (Just tok) (Just uid) spar) ((== 404) . statusCode)
                !!! const 404 === statusCode

        it "should free externalId and everything else in the scim user for re-use" $ do
            (tok, _) <- registerIdPAndScimToken
            user <- randomScimUser
            storedUser <- createUser tok user
            let uid :: UserId = scimUserId storedUser
            spar <- view teSpar
            deleteUser_ (Just tok) (Just uid) spar
                !!! const 204 === statusCode
            eventually (createUser_ (Just tok) user spar) ((== 201) . statusCode)
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
            getUser_ (Just tok) uid spar
                !!! const 404 === statusCode

        it "whether implemented or not, does *NOT EVER* respond with 5xx!" $ do
            env <- ask
            user <- randomScimUser
            (tok, _) <- registerIdPAndScimToken
            storedUser <- createUser tok user
            deleteUser_ (Just tok) (Just $ scimUserId storedUser) (env ^. teSpar)
                !!! assertTrue_ (inRange (200, 499) . statusCode)
