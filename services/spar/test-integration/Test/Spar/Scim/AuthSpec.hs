{-# LANGUAGE OverloadedStrings #-}

-- | SCIM API authentication and SCIM token tests.
--
-- Note: we create a new team in each test, so we don't have to do any kind of cleanup.
module Test.Spar.Scim.AuthSpec (spec) where

import Imports
import Bilge
import Bilge.Assert
import Control.Lens
import Spar.Scim
import Spar.Types (ScimTokenInfo(..))
import Util

import qualified Galley.Types.Teams as Galley

-- | Tests for authentication and operations with provisioning tokens ('ScimToken's).
spec :: SpecWith TestEnv
spec = do
    specCreateToken
    specDeleteToken
    specListTokens
    describe "Miscellaneous" $ do
        it "doesn't allow SCIM operations without a SCIM token" $ testAuthIsNeeded

----------------------------------------------------------------------------
-- Token creation

-- | Tests for @POST /auth-tokens@.
specCreateToken :: SpecWith TestEnv
specCreateToken = describe "POST /auth-tokens" $ do
    it "works" $ testCreateToken
    it "respects the token limit" $ testTokenLimit
    it "requires the team to have an IdP" $ testIdPIsNeeded
    it "authorizes only team owner" $ testCreateTokenAuthorizesOnlyTeamOwner

-- | Test that token creation is sane:
--
-- * Token can be created.
-- * SCIM API can be used with that token.
testCreateToken :: TestSpar ()
testCreateToken = do
    env <- ask
    -- Create a token
    (owner, _, _) <- registerTestIdP
    CreateScimTokenResponse token _ <-
        createToken owner CreateScimToken
            { createScimTokenDescr = "testCreateToken" }
    -- Try to do @GET /Users@ and check that it succeeds
    listUsers_ (Just token) Nothing (env ^. teSpar)
        !!! const 200 === statusCode

-- | Test that only up to @maxScimTokens@ can be created.
--
-- We assume that in tests, the token limit is 2.
testTokenLimit :: TestSpar ()
testTokenLimit = do
    env <- ask
    -- Create two tokens
    (owner, _, _) <- registerTestIdP
    _ <- createToken owner CreateScimToken
             { createScimTokenDescr = "testTokenLimit / #1" }
    _ <- createToken owner CreateScimToken
             { createScimTokenDescr = "testTokenLimit / #2" }
    -- Try to create the third token and see that it fails
    createToken_ owner CreateScimToken
        { createScimTokenDescr = "testTokenLimit / #3" }
        (env ^. teSpar)
        !!! const 403 === statusCode

-- | Test that a token can't be created for a team without an IdP.
--
-- (We don't support SCIM without SSO.)
testIdPIsNeeded :: TestSpar ()
testIdPIsNeeded = do
    env <- ask
    -- Create a new team and don't associate an IdP with it
    (userid, _teamid) <- runHttpT (env ^. teMgr) $
        createUserWithTeam (env ^. teBrig) (env ^. teGalley)
    -- Creating a token should fail now
    createToken_
        userid
        CreateScimToken { createScimTokenDescr = "testIdPIsNeeded" }
        (env ^. teSpar)
        !!! const 400 === statusCode

-- | Test that a token can only be created as a team owner
testCreateTokenAuthorizesOnlyTeamOwner :: TestSpar ()
testCreateTokenAuthorizesOnlyTeamOwner =
  do
    env <- ask
    (_, teamId,_) <- registerTestIdP
    teamMemberId <- runHttpT (env ^. teMgr)
      $ createTeamMember
        (env ^. teBrig)
        (env ^. teGalley)
        teamId
        (Galley.rolePermissions Galley.RoleMember)
    createToken_
      teamMemberId
      (CreateScimToken
       { createScimTokenDescr = "testCreateToken"
       })
      (env ^. teSpar)
      !!! const 403
      === statusCode


----------------------------------------------------------------------------
-- Token listing

-- | Tests for @GET /auth-tokens@.
specListTokens :: SpecWith TestEnv
specListTokens = describe "GET /auth-tokens" $ do
    it "works" $ testListTokens

-- | Test that existing tokens can be listed.
testListTokens :: TestSpar ()
testListTokens = do
   -- Create two tokens
   (owner, _, _) <- registerTestIdP
   _ <- createToken owner CreateScimToken
            { createScimTokenDescr = "testListTokens / #1" }
   _ <- createToken owner CreateScimToken
            { createScimTokenDescr = "testListTokens / #2" }
   -- Check that the token is on the list
   list <- scimTokenListTokens <$> listTokens owner
   liftIO $ map stiDescr list `shouldBe`
       ["testListTokens / #1", "testListTokens / #2"]

----------------------------------------------------------------------------
-- Token deletion

-- | Tests for @DELETE /auth-tokens/:id@.
specDeleteToken :: SpecWith TestEnv
specDeleteToken = describe "DELETE /auth-tokens/:id" $ do
    it "makes the token unusable" $ testDeletedTokensAreUnusable
    it "makes the token unlistable" $ testDeletedTokensAreUnlistable

-- | Test that when a token is deleted, it's no longer usable.
testDeletedTokensAreUnusable :: TestSpar ()
testDeletedTokensAreUnusable = do
    env <- ask
    -- Create a token
    (owner, _, _) <- registerTestIdP
    CreateScimTokenResponse token tokenInfo <-
        createToken owner CreateScimToken
            { createScimTokenDescr = "testDeletedTokensAreUnusable" }
    -- An operation with the token should succeed
    listUsers_ (Just token) Nothing (env ^. teSpar)
        !!! const 200 === statusCode
    -- Delete the token and now the operation should fail
    deleteToken owner (stiId tokenInfo)
    listUsers_ (Just token) Nothing (env ^. teSpar)
        !!! const 401 === statusCode

-- | Test that when a token is deleted, it no longer appears in the result of @GET
-- /auth-tokens@.
testDeletedTokensAreUnlistable :: TestSpar ()
testDeletedTokensAreUnlistable = do
   -- Create a token
   (owner, _, _) <- registerTestIdP
   CreateScimTokenResponse _ tokenInfo <-
       createToken owner CreateScimToken
           { createScimTokenDescr = "testDeletedTokensAreUnlistable" }
   -- Delete the token
   deleteToken owner (stiId tokenInfo)
   -- Check that the token is not on the list
   list <- scimTokenListTokens <$> listTokens owner
   liftIO $ list `shouldBe` []

----------------------------------------------------------------------------
-- Miscellaneous tests

-- | Test that without a token, the SCIM API can't be used.
testAuthIsNeeded :: TestSpar ()
testAuthIsNeeded = do
    env <- ask
    -- Try to do @GET /Users@ without a token and check that it fails
    listUsers_ Nothing Nothing (env ^. teSpar)
        !!! const 401 === statusCode
