{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE LambdaCase          #-}

-- | Integration tests for the main SCIM API (@\/scim\/v2@).
--
-- Functionality that lives under @/scim@ but doesn't belong to the SCIM protocol should be
-- tested in separate modules.
module Test.Spar.ScimSpec (spec) where

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


-- | Testing the main SCIM API. Currently only 'specUsers'.
spec :: SpecWith TestEnv
spec = do
    specUsers

-- | Tests for @\/scim\/v2\/Users@.
specUsers :: SpecWith TestEnv
specUsers = describe "/scim/v2/Users" $ do
    describe "POST /Users" $ do
        it "creates a user in an existing team" $ do
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

        it "requires externalId to be present" $ do
            env <- ask
            -- Create a user with a missing @externalId@ and check that it fails
            user <- randomScimUser
            let user' = user { Scim.User.externalId = Nothing }
            (tok, _) <- registerIdPAndScimToken
            createUser_ (Just tok) user' (env ^. teSpar)
                !!! const 400 === statusCode

        it "gives created user a valid 'SAML.UserRef' for SSO." $ do
            pending

        it "attributes of brig user, scim user, saml user are mapped as documented." $ do
            pending

        it "writes all the stuff to all the places" $ do
            pendingWith "factor this out of the PUT tests we already wrote."

    describe "GET /Users" $ do
        it "lists all users in a team" $ do
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
        it "finds a SCIM-provisioned user by username" $
            pending
        it "finds a non-SCIM-provisioned user by username" $
            pending
        it "doesn't list deleted users" $ do
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

    describe "GET /Users/:id" $ do
        it "finds a SCIM-provisioned user" $ do
            -- Create a user via SCIM
            user <- randomScimUser
            (tok, _) <- registerIdPAndScimToken
            storedUser <- createUser tok user
            -- Check that the SCIM-provisioned user can be fetched
            storedUser' <- getUser tok (scimUserId storedUser)
            liftIO $ storedUser' `shouldBe` storedUser
        it "does NOT find a non-SCIM-provisioned user" $ do
            pendingWith "TODO: fails because the user has no handle (UPDATE: \
                        \it *should* fail, too, just need to make sure it's \
                        \for the right reasons.)"
            {-
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
        it "doesn't find a user that's not part of the team" $ do
            pending
            -- create another team and another user in it
            -- check that this user can not be found in the "wrong" team
        it "doesn't find a deleted user" $ do
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

    describe "PUT /Users" $ do
        it "responds with 4xx (just making sure...)" $ do
            env <- ask
            user <- randomScimUser
            (tok, _) <- registerIdPAndScimToken
            updateUser_ (Just tok) Nothing user (env ^. teSpar)
                !!! assertTrue_ (inRange (400, 499) . statusCode)

    describe "PUT /Users/:id" $ do
        it "updates the user attributes in scim_user" $ do
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

        it "works fine when neither name nor handle are changed" $ do
            -- NB: this test is needed because if PUT /Users is implemented in such a way that
            -- it always tries to set the name and handle, it might fail because e.g. the
            -- handle is "already claimed".
            --
            -- Create a user via SCIM
            user <- randomScimUser
            (tok, _) <- registerIdPAndScimToken
            storedUser <- createUser tok user
            let userid = scimUserId storedUser
            -- Overwrite the user with another randomly-generated user who has the same name
            -- and handle
            user' <- randomScimUser <&> \u ->
                u { Scim.User.userName = Scim.User.userName user
                  , Scim.User.displayName = Scim.User.displayName user
                  }
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

        it "updates 'SAML.UserRef' in spar" $ do
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

        it "creates a matching Brig user (with 'SSOIdentity' correctly set)" $ do
            user <- randomScimUser
            (tok, (_, _, idp)) <- registerIdPAndScimToken
            storedUser <- createUser tok user
            user' <- randomScimUser
            let userid = scimUserId storedUser
            _ <- updateUser tok userid user'
            validScimUser <- either (error . show) pure $ validateScimUser' (Just idp) user'
            brigUser      <- maybe (error "no brig user") pure =<< getSelf userid
            brigUser `userShouldMatch` validScimUser

        context "scim_user has no entry with this id" $ do
            it "fails" $ do
                pending

        context "brig user is updated" $ do
            it "does NOT mirror this in the scim user" $ do
                pendingWith "this is arguably not great behavior, but \
                            \i'm not sure we want to implement \
                            \synchronisation from brig to spar right now?"

            it "updates to scim user will overwrite these updates" $ do
                pendingWith "that's probably what we want?"

    describe "DELETE /Users" $ do
        it "responds with 404 (just making sure...)" $ do
            pending

    describe "DELETE /Users/:id" $ do
        it "sets the 'deleted' flag in brig, and does nothing otherwise." $ do
            pendingWith "really?  how do we destroy the data then, and when?"

    describe "CRUD operations maintain invariants in mapScimToBrig, mapBrigToScim." $ do
        it "..." $ do
            pendingWith "this is a job for quickcheck-state-machine"

    describe "validateScimUser'" $ do
        it "works" $ do
            pendingWith "write a list of unit tests here that make the mapping explicit, exhaustive, and easy to read."
