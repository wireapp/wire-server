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

import Imports
import Bilge
import Bilge.Assert
import Brig.Types.User
import Control.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Text (pack, unpack)
import System.Random
import Spar.Types (ScimToken)
import Spar.SCIM (CreateScimToken(..))
import Util
import Web.HttpApiData (toHeader)

import qualified Web.SCIM.Class.User              as SCIM
import qualified Web.SCIM.Schema.ListResponse     as SCIM
import qualified Web.SCIM.Schema.Common           as SCIM
import qualified Web.SCIM.Schema.Meta             as SCIM
import qualified Web.SCIM.Filter                  as SCIM

import qualified Web.SCIM.Schema.User             as SCIM.User

import qualified Data.Aeson as Aeson


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
            user <- randomUser
            scimStoredUser <- createUser user
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
            env <- ask
            -- Create a user via SCIM
            user <- randomUser
            storedUser <- createUser user
            -- Get all users via SCIM
            users <- listUsers Nothing
            -- Check that the SCIM user is present
            liftIO $ users `shouldContain` [storedUser]
            -- Check that the (non-SCIM-provisioned) team owner is present
            liftIO $ users `shouldSatisfy`
                any ((== env^.teUserId) . scimUserId)
        it "finds a SCIM-provisioned user by username" $
            pending
        it "finds a non-SCIM-provisioned user by username" $
            pending

    describe "GET /Users/:id" $ do
        it "finds a SCIM-provisioned user" $ do
            -- Create a user via SCIM
            user <- randomUser
            storedUser <- createUser user
            -- Check that the SCIM-provisioned user can be fetched
            storedUser' <- getUser (scimUserId storedUser)
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

-- | Generate a SCIM user with a random name and handle.
randomUser :: TestSpar SCIM.User.User
randomUser = do
    suffix <- pack <$> replicateM 5 (liftIO (randomRIO ('0', '9')))
    pure $ SCIM.User.empty
        { SCIM.User.userName    = "scimuser_" <> suffix
        , SCIM.User.displayName = Just ("Scim User #" <> suffix)
        , SCIM.User.externalId  = Just ("scimuser_extid_" <> suffix)
        }

specTokens :: SpecWith TestEnv
specTokens = describe "operations with provisioning tokens" $ do
    describe "POST /auth-tokens" $ do
        it "creates a usable token" $ do
            env <- ask
            -- Create a token
            token <- createToken CreateScimToken
                { createScimTokenDescription = "token creation test" }
            -- Try to execute a SCIM operation without a token and check
            -- that it fails
            listUsers_ Nothing Nothing (env ^. teSpar)
                !!! const 401 === statusCode
            -- Try to execute the same SCIM operation with the generated
            -- token; it should succeed now
            listUsers_ (Just token) Nothing (env ^. teSpar)
                !!! const 200 === statusCode
            -- Cleanup
            deleteToken token
        it "respects the token limit (2 for integration tests)" $ do
            env <- ask
            -- Try to create two tokens; the second attempt should fail
            token1 <- createToken CreateScimToken
                { createScimTokenDescription = "token limit test / #1" }
            createToken_ (env ^. teUserId) CreateScimToken
                { createScimTokenDescription = "token limit test / #2" }
                (env ^. teSpar)
                !!! const 403 === statusCode
            -- Cleanup
            deleteToken token1

    describe "DELETE /auth-tokens/:token" $ do
        it "makes the token unusable" $ do
            env <- ask
            -- Create a token
            token <- createToken CreateScimToken
                { createScimTokenDescription = "token deletion test" }
            -- An operation with the token should succeed
            listUsers_ (Just token) Nothing (env ^. teSpar)
                !!! const 200 === statusCode
            -- Delete the token and now the operation should fail
            deleteToken token
            listUsers_ (Just token) Nothing (env ^. teSpar)
                !!! const 401 === statusCode

----------------------------------------------------------------------------
-- API wrappers

-- | Create a user in the default 'TestEnv' team.
createUser
    :: HasCallStack
    => SCIM.User.User
    -> TestSpar SCIM.StoredUser
createUser user = do
    env <- ask
    r <- createUser_
             (Just (env ^. teScimToken))
             user
             (env ^. teSpar)
         <!! const 201 === statusCode
    pure (decodeBody' r)

-- | List all users in the default 'TestEnv' team.
listUsers
    :: HasCallStack
    => Maybe SCIM.Filter
    -> TestSpar [SCIM.StoredUser]
listUsers mbFilter = do
    env <- ask
    r <- listUsers_
             (Just (env ^. teScimToken))
             mbFilter
             (env ^. teSpar)
         <!! const 200 === statusCode
    let r' = decodeBody' r
    when (SCIM.totalResults r' /= length (SCIM.resources r')) $
        error "listUsers: got a paginated result, but pagination \
              \is not supported yet"
    pure (SCIM.resources r')

-- | Get a user belonging to the default 'TestEnv' team.
getUser
    :: HasCallStack
    => UserId
    -> TestSpar SCIM.StoredUser
getUser userid = do
    env <- ask
    r <- getUser_
             (Just (env ^. teScimToken))
             userid
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

-- | Create a SCIM token for the default 'TestEnv' team.
createToken
    :: HasCallStack
    => CreateScimToken
    -> TestSpar ScimToken
createToken payload = do
    env <- ask
    r <- createToken_
             (env ^. teUserId)
             payload
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

-- | Delete a SCIM token belonging to the default 'TestEnv' team.
deleteToken
    :: HasCallStack
    => ScimToken                -- ^ Token to delete
    -> TestSpar ()
deleteToken token = do
    env <- ask
    deleteToken_
        (env ^. teUserId)
        token
        (env ^. teSpar)
        !!! const 204 === statusCode

----------------------------------------------------------------------------
-- "Raw" API requests

-- | Create a user.
createUser_
    :: Maybe ScimToken          -- ^ Authentication
    -> SCIM.User.User           -- ^ User data
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
createUser_ auth user spar_ = do
    -- NB: we don't use 'mkEmailRandomLocalSuffix' here, because emails
    -- shouldn't be submitted via SCIM anyway.
    call . post $
        ( spar_
        . paths ["scim", "v2", "Users"]
        . scimAuth auth
        . contentScim
        . body (RequestBodyLBS . Aeson.encode $ user)
        . acceptScim
        )

-- | List all users.
listUsers_
    :: Maybe ScimToken          -- ^ Authentication
    -> Maybe SCIM.Filter        -- ^ Predicate to filter the results
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
listUsers_ auth mbFilter spar_ = do
    call . get $
        ( spar_
        . paths ["scim", "v2", "Users"]
        . queryItem' "filter" (toByteString' . SCIM.renderFilter <$> mbFilter)
        . scimAuth auth
        . acceptScim
        )

-- | Get one user.
getUser_
    :: Maybe ScimToken          -- ^ Authentication
    -> UserId                   -- ^ User
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
getUser_ auth userid spar_ = do
    call . get $
        ( spar_
        . paths ["scim", "v2", "Users", toByteString' userid]
        . scimAuth auth
        . acceptScim
        )

-- | Create a SCIM token.
createToken_
    :: UserId                   -- ^ User
    -> CreateScimToken
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
createToken_ userid payload spar_ = do
    call . post $
        ( spar_
        . paths ["scim", "auth-tokens"]
        . zUser userid
        . contentJson
        . body (RequestBodyLBS . Aeson.encode $ payload)
        . acceptJson
        )

-- | Delete a SCIM token.
deleteToken_
    :: UserId                   -- ^ User
    -> ScimToken                -- ^ Token to delete
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
deleteToken_ userid token spar_ = do
    call . delete $
        ( spar_
        . paths ["scim", "auth-tokens"]
        . queryItem "token" (toByteString' token)
        . zUser userid
        )

----------------------------------------------------------------------------
-- Utilities

-- | Add SCIM authentication to a request.
scimAuth :: Maybe ScimToken -> Request -> Request
scimAuth Nothing = id
scimAuth (Just auth) = header "Authorization" (toHeader auth)

-- | Signal that the body is an SCIM payload.
contentScim :: Request -> Request
contentScim = content "application/scim+json"

-- | Signal that the response type is expected to be an SCIM payload.
acceptScim :: Request -> Request
acceptScim = accept "application/scim+json"

-- | Get ID of a user returned from SCIM.
scimUserId :: SCIM.StoredUser -> UserId
scimUserId storedUser = either err id (readEither id_)
  where
    id_ = unpack (SCIM.id (SCIM.thing storedUser))
    err e = error $ "scimUserId: couldn't parse ID " ++ id_ ++ ": " ++ e

-- | Check that some properties match between an SCIM user and a Brig user.
userShouldMatch
    :: (HasCallStack, MonadIO m)
    => SCIM.StoredUser -> User -> m ()
userShouldMatch scimStoredUser brigUser = liftIO $ do
    let scimUser = SCIM.value (SCIM.thing scimStoredUser)
    scimUserId scimStoredUser `shouldBe`
        userId brigUser
    Just (Handle (SCIM.User.userName scimUser)) `shouldBe`
        userHandle brigUser
    fmap Name (SCIM.User.displayName scimUser) `shouldBe`
        Just (userName brigUser)
