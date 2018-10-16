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
import Brig.Types.User
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Conversion
import Data.Id
import Data.Monoid
import Lens.Micro
import Data.UUID as UUID hiding (null, fromByteString)
import Data.Text (pack)
import System.Random
-- import Control.Retry
-- import Data.List (isInfixOf)
-- import Data.Maybe
-- import Data.String.Conversions
-- import Data.UUID.V4 as UUID
-- import Galley.Types.Teams as Galley
-- import Prelude hiding (head)
-- import SAML2.WebSSO as SAML
-- import SAML2.WebSSO.Test.MockResponse
-- import Spar.Types
-- import Spar.API.Types
import Util

import qualified Web.SCIM.Class.User              as SCIM
import qualified Web.SCIM.Class.Auth              as SCIM
import qualified Web.SCIM.Schema.Common           as SCIM
import qualified Web.SCIM.Schema.Meta             as SCIM

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
            suffix <- pack <$> replicateM 6 (liftIO (randomRIO ('a', 'z')))
            let handl = "bob_scim_" <> suffix
            let user = SCIM.User.empty
                  { SCIM.User.userName = handl
                  , SCIM.User.displayName = Just "Bob Scim"
                  , SCIM.User.externalId = Just ("bob_scim_external_" <> suffix)
                  }
            scimUser <- createUser user
            let userid = SCIM.id (SCIM.thing scimUser)
            -- Check that this user is present in Brig
            brigUser <- fmap decodeBody' . call . get $
                ( (env ^. teBrig)
                . header "Z-User" (toByteString' userid)
                . path "/self"
                . expect2xx
                )
            -- Check that the fields were set correctly
            liftIO $ do
                idToText (userId brigUser) `shouldBe` userid
                userHandle brigUser `shouldBe` Just (Handle handl)
                userName brigUser `shouldBe` Name "Bob Scim"

    describe "GET /Users" $ do
        it "lists all users in a team" $ do
            pending
            -- check that both normally created and SCIM-created user are listable

    describe "GET /Users/:userName" $ do
        it "finds the SCIM-provisioned user by username" $ do
            pending
            -- check that the SCIM-provisioned user is get-table
        it "finds the pre-existing user by username" $ do
            pending
            -- check that the pre-existing user is get-table
        it "doesn't find a user that's not part of the team" $ do
            pending
            -- create another team and another user in it
            -- check that this user can not be found in the "wrong" team

----------------------------------------------------------------------------
-- High-level SCIM API

-- | Create a user in the default 'TestEnv' team.
createUser
    :: SCIM.User.User
    -> TestSpar SCIM.StoredUser
createUser user = do
    env <- ask
    r <- postUser
             (Just (env ^. teScimAdmin))
             (env ^. teTeamId)
             user
             (env ^. teSpar)
         <!! const 201 === statusCode
    pure (decodeBody' r)

----------------------------------------------------------------------------
-- Low-level SCIM API

-- | Create a user.
postUser
    :: Maybe SCIM.SCIMAuthData  -- ^ Admin credentials for authentication
    -> TeamId                   -- ^ Team
    -> SCIM.User.User           -- ^ User data
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
postUser auth teamid user spar_ = do
    -- NB: we don't use 'mkEmailRandomLocalSuffix' here, because emails
    -- shouldn't be submitted via SCIM anyway.
    let p = RequestBodyLBS . Aeson.encode $ user
    call . post $
        ( spar_
        . paths ["scim", toByteString' teamid, "Users"]
        . scimAuth auth
        . contentJson
        . body p
        )

----------------------------------------------------------------------------
-- Utilities

-- | Add SCIM authentication credentials to a request.
scimAuth :: Maybe SCIM.SCIMAuthData -> Request -> Request
scimAuth Nothing = id
scimAuth (Just auth) =
    applyBasicAuth
        (UUID.toASCIIBytes (SCIM.scimAdmin auth))
        (SCIM.scimPassword auth)
