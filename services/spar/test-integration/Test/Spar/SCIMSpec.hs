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

module Test.Spar.SCIMSpec where

import Bilge
import Bilge.Assert
import Brig.Types
import Control.Lens
import Control.Retry
import Data.ByteString.Conversion
import Imports
import Spar.SCIM (CreateScimToken(..), CreateScimTokenResponse(..), ScimTokenList(..))
import Spar.Types (ScimTokenInfo(..))
import Util


spec :: SpecWith TestEnv
spec = do
    specUsers
    specTokens

specUsers :: SpecWith TestEnv
specUsers = describe "operations with users" $ do
    describe "POST /Users" $ do
        it "creates a user in an existing team" $ do
            env <- ask
            -- Create a user via SCIM
            user <- randomSCIMUser
            (tok, _) <- registerIdPAndSCIMToken
            scimStoredUser <- createUser tok user
            let userid = scimUserId scimStoredUser
            -- Check that this user is present in Brig and that Brig's view
            -- of the user matches SCIM's view of the user
            brigUser <- fmap decodeBody' . call . get $
                ( (env ^. teBrig)
                . header "Z-User" (toByteString' userid)
                . path "/self"
                . expect2xx
                )
            scimStoredUser `userShouldMatch` brigUser

    describe "GET /Users" $ do
        it "lists all users in a team" $ do
            -- Create a user via SCIM
            user <- randomSCIMUser
            (tok, (owner, _, _)) <- registerIdPAndSCIMToken
            storedUser <- createUser tok user
            -- Get all users via SCIM
            users <- listUsers tok Nothing
            -- Check that the SCIM user is present
            liftIO $ users `shouldContain` [storedUser]
            -- Check that the (non-SCIM-provisioned) team owner is present
            liftIO $ users `shouldSatisfy`
                any ((== owner) . scimUserId)
        it "finds a SCIM-provisioned user by username" $
            pending
        it "finds a non-SCIM-provisioned user by username" $
            pending
        it "doesn't list deleted users" $ do
            env <- ask
            -- Create a user via SCIM
            user <- randomSCIMUser
            (tok, _) <- registerIdPAndSCIMToken
            storedUser <- createUser tok user
            let userid = scimUserId storedUser
            -- Delete the user (TODO: do it via SCIM) and wait until the
            -- user is truly gone
            call $ deleteUser (env ^. teBrig) userid
            recoverAll (exponentialBackoff 30000 <> limitRetries 5) $ \_ -> do
                profile <- call $ getSelfProfile (env ^. teBrig) userid
                liftIO $ selfUser profile `shouldSatisfy` userDeleted
            -- Get all users
            users <- listUsers tok Nothing
            -- Check that the user is absent
            liftIO $ users `shouldSatisfy` all ((/= userid) . scimUserId)

    describe "GET /Users/:id" $ do
        it "finds a SCIM-provisioned user" $ do
            -- Create a user via SCIM
            user <- randomSCIMUser
            (tok, _) <- registerIdPAndSCIMToken
            storedUser <- createUser tok user
            -- Check that the SCIM-provisioned user can be fetched
            storedUser' <- getUser tok (scimUserId storedUser)
            liftIO $ storedUser' `shouldBe` storedUser
        it "finds a non-SCIM-provisioned user" $ do
            pendingWith "TODO: fails because the user has no handle"
            {-
            env <- ask
            -- Check that the (non-SCIM-provisioned) team owner can be fetched
            -- and that the data from Brig matches
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
            user <- randomSCIMUser
            (tok, _) <- registerIdPAndSCIMToken
            storedUser <- createUser tok user
            let userid = scimUserId storedUser
            -- Delete the user (TODO: do it via SCIM) and wait until the
            -- user is truly gone
            call $ deleteUser (env ^. teBrig) userid
            recoverAll (exponentialBackoff 30000 <> limitRetries 5) $ \_ -> do
                profile <- call $ getSelfProfile (env ^. teBrig) userid
                liftIO $ selfUser profile `shouldSatisfy` userDeleted
            -- Try to find the user
            getUser_ (Just tok) userid (env ^. teSpar)
                !!! const 404 === statusCode

specTokens :: SpecWith TestEnv
specTokens = xdescribe "operations with provisioning tokens" $ do
    describe "POST /auth-tokens" $ do
        it "creates a usable token" $ do
            env <- ask
            -- Create a token
            (owner, _, _) <- registerTestIdP
            CreateScimTokenResponse token tokenInfo <-
                createToken owner CreateScimToken
                    { createScimTokenDescr = "token creation test" }
            -- Try to execute a SCIM operation without a token and check
            -- that it fails
            listUsers_ Nothing Nothing (env ^. teSpar)
                !!! const 401 === statusCode
            -- Try to execute the same SCIM operation with the generated
            -- token; it should succeed now
            listUsers_ (Just token) Nothing (env ^. teSpar)
                !!! const 200 === statusCode
            -- Cleanup
            deleteToken owner (stiId tokenInfo)

        it "respects the token limit (2 for integration tests)" $ do
            env <- ask
            -- Try to create two more tokens (in addition to the already
            -- existing token that's created in 'mkEnv'). Creating the
            -- second token should succeed, and creating the third token
            -- should fail.
            (owner, _, _) <- registerTestIdP
            CreateScimTokenResponse _ tokenInfo1 <-
                createToken owner CreateScimToken
                    { createScimTokenDescr = "token limit test / #1" }
            createToken_ owner CreateScimToken
                { createScimTokenDescr = "token limit test / #2" }
                (env ^. teSpar)
                !!! const 403 === statusCode
            -- Cleanup
            deleteToken owner (stiId tokenInfo1)

        it "doesn't create a token for a team without IdP" $ do
            env <- ask
            -- Create a new team and don't associate an IdP with it
            (userid, _teamid) <- runHttpT (env ^. teMgr) $
                createUserWithTeam (env ^. teBrig) (env ^. teGalley)
            -- Creating a token should fail now
            createToken_
                userid
                CreateScimToken { createScimTokenDescr = "IdP-less team test" }
                (env ^. teSpar)
                !!! const 400 === statusCode

    describe "DELETE /auth-tokens/:id" $ do
        it "makes the token unusable" $ do
            env <- ask
            -- Create a token
            (owner, _, _) <- registerTestIdP
            CreateScimTokenResponse token tokenInfo <-
                createToken owner CreateScimToken
                    { createScimTokenDescr = "token deletion test" }
            -- An operation with the token should succeed
            listUsers_ (Just token) Nothing (env ^. teSpar)
                !!! const 200 === statusCode
            -- Delete the token and now the operation should fail
            deleteToken owner (stiId tokenInfo)
            listUsers_ (Just token) Nothing (env ^. teSpar)
                !!! const 401 === statusCode

    describe "GET /auth-tokens" $ do
        it "lists tokens" $ do
            -- Create a token
            (owner, _, _) <- registerTestIdP
            CreateScimTokenResponse _ tokenInfo <-
                createToken owner CreateScimToken
                    { createScimTokenDescr = "token listing test" }
            -- Both the default team token and this token should be present
            do list <- scimTokenListTokens <$> listTokens owner
               liftIO $ map stiDescr list `shouldBe`
                   ["_teScimToken test token", "token listing test"]
            -- Delete the token and now it shouldn't be on the list
            deleteToken owner (stiId tokenInfo)
            do list <- scimTokenListTokens <$> listTokens owner
               liftIO $ map stiDescr list `shouldBe`
                   ["_teScimToken test token"]
