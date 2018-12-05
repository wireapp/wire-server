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

module Util.SCIM where

import Bilge
import Bilge.Assert
import Brig.Types.User
import Control.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Text (pack, unpack)
import Imports
import Spar.SCIM (CreateScimToken(..), CreateScimTokenResponse(..), ScimTokenList(..))
import Spar.Types (ScimToken)
import System.Random
import Util.Core
import Util.Types
import Web.HttpApiData (toHeader)

import qualified Data.Aeson                       as Aeson
import qualified Web.SCIM.Class.User              as SCIM
import qualified Web.SCIM.Filter                  as SCIM
import qualified Web.SCIM.Schema.Common           as SCIM
import qualified Web.SCIM.Schema.ListResponse     as SCIM
import qualified Web.SCIM.Schema.Meta             as SCIM
import qualified Web.SCIM.Schema.User             as SCIM.User


-- | Generate a SCIM user with a random name and handle.
randomSCIMUser :: TestSpar SCIM.User.User
randomSCIMUser = do
    suffix <- pack <$> replicateM 5 (liftIO (randomRIO ('0', '9')))
    pure $ SCIM.User.empty
        { SCIM.User.userName    = "scimuser_" <> suffix
        , SCIM.User.displayName = Just ("Scim User #" <> suffix)
        , SCIM.User.externalId  = Just ("scimuser_extid_" <> suffix)
        }

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
    -> TestSpar CreateScimTokenResponse
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
    => ScimTokenId                -- ^ Token to delete
    -> TestSpar ()
deleteToken tokenid = do
    env <- ask
    deleteToken_
        (env ^. teUserId)
        tokenid
        (env ^. teSpar)
        !!! const 204 === statusCode

-- | List SCIM tokens belonging to the default 'TestEnv' team.
listTokens
    :: HasCallStack
    => TestSpar ScimTokenList
listTokens = do
    env <- ask
    r <- listTokens_
             (env ^. teUserId)
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

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
    -> ScimTokenId              -- ^ Token to delete
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
deleteToken_ userid tokenid spar_ = do
    call . delete $
        ( spar_
        . paths ["scim", "auth-tokens"]
        . queryItem "id" (toByteString' tokenid)
        . zUser userid
        )

-- | List SCIM tokens.
listTokens_
    :: UserId                   -- ^ User
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
listTokens_ userid spar_ = do
    call . get $
        ( spar_
        . paths ["scim", "auth-tokens"]
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
