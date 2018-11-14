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

----------------------------------------------------------------------------
-- High-level SCIM API

-- | Create a user in the default 'TestEnv' team.
createUser
    :: SCIM.User.User
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
    :: Maybe SCIM.Filter
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
    :: UserId
    -> TestSpar SCIM.StoredUser
getUser userid = do
    env <- ask
    r <- getUser_
             (Just (env ^. teScimToken))
             userid
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

----------------------------------------------------------------------------
-- Low-level SCIM API

-- | Create a user.
createUser_
    :: Maybe ScimToken          -- ^ Authentication
    -> SCIM.User.User           -- ^ User data
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
createUser_ auth user spar_ = do
    -- NB: we don't use 'mkEmailRandomLocalSuffix' here, because emails
    -- shouldn't be submitted via SCIM anyway.
    let p = RequestBodyLBS . Aeson.encode $ user
    call . post $
        ( spar_
        . paths ["scim", "v2", "Users"]
        . scimAuth auth
        . contentScim
        . body p
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
