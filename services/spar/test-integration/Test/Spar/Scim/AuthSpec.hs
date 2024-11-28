{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- | SCIM API authentication and SCIM token tests.
--
-- Note: we create a new team in each test, so we don't have to do any kind of cleanup.
module Test.Spar.Scim.AuthSpec
  ( spec,
  )
where

import Bilge
import Bilge.Assert
import Cassandra as Cas
import Control.Lens
import Data.Aeson (encode)
import qualified Data.ByteString.Base64 as ES
import Data.ByteString.Conversion (toByteString')
import qualified Data.Code as Code
import Data.Id (ScimTokenId, TeamId, UserId, randomId)
import Data.Misc
import Data.Range (unsafeRange)
import Data.String.Conversions
import Data.Text.Ascii (AsciiChars (validate))
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Imports
import OpenSSL.Random (randBytes)
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.Util as SAML
import Spar.Scim
import Text.RawString.QQ (r)
import Util
import Wire.API.Team.Feature (featureNameBS)
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.Member (rolePermissions)
import Wire.API.Team.Role
import Wire.API.User (userEmail)
import qualified Wire.API.User as Public
import Wire.API.User.Identity

-- | Tests for authentication and operations with provisioning tokens ('ScimToken's).
spec :: SpecWith TestEnv
spec = do
  specCreateToken
  specDeleteToken
  specListTokens
  describe "Miscellaneous" $ do
    it "testAuthIsNeeded - doesn't allow SCIM operations with invalid or missing SCIM token" testAuthIsNeeded

----------------------------------------------------------------------------
-- Token creation

-- | Tests for @POST /auth-tokens@.
specCreateToken :: SpecWith TestEnv
specCreateToken = describe "POST /auth-tokens" $ do
  it "works" testCreateToken
  it "respects the token limit" testTokenLimit
  it "requires the team to have no more than one IdP" testNumIdPs
  it "testCreateTokenAuthorizesOnlyAdmins - authorizes only admins and owners" testCreateTokenAuthorizesOnlyAdmins
  it "requires a password" testCreateTokenRequiresPassword
  it "testCreateTokenWithVerificationCode - works with verification code" testCreateTokenWithVerificationCode

-- FUTUREWORK: we should also test that for a password-less user, e.g. for an SSO user,
-- reauthentication is not required. We currently (2019-03-05) can't test that because
-- only team owners with an email address can do SCIM token operations (which is something
-- we should change in the future).

-- | Test that token creation is sane:
--
-- * Token can be created.
-- * SCIM API can be used with that token.
testCreateToken :: TestSpar ()
testCreateToken = do
  env <- ask
  -- Create a token
  (owner, _tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  CreateScimTokenResponseV6 token _ <-
    createToken
      owner
      CreateScimToken
        { description = "testCreateToken",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  -- Try to do @GET /Users@ and check that it succeeds
  let fltr = filterBy "externalId" "67c196a0-cd0e-11ea-93c7-ef550ee48502"
  listUsers_ (Just token) (Just fltr) (env ^. teSpar)
    !!! const 200 === statusCode

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test positive case but also that a SCIM token cannot be created with wrong
-- or missing second factor email verification code when this feature is enabled
testCreateTokenWithVerificationCode :: TestSpar ()
testCreateTokenWithVerificationCode = do
  env <- ask
  (owner, teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  unlockFeature (env ^. teGalley) teamId
  setSndFactorPasswordChallengeStatus (env ^. teGalley) teamId Public.FeatureStatusEnabled
  user <- getUserBrig owner
  let email = fromMaybe undefined (userEmail =<< user)

  let reqMissingCode = CreateScimToken "testCreateToken" (Just defPassword) Nothing Nothing Nothing
  createTokenFailsWith owner reqMissingCode 403 "code-authentication-required"

  void $ requestVerificationCode (env ^. teBrig) email Public.CreateScimToken
  let wrongCode = Code.Value $ unsafeRange (fromRight undefined (validate "123456"))
  let reqWrongCode = CreateScimToken "testCreateToken" (Just defPassword) (Just wrongCode) Nothing Nothing
  createTokenFailsWith owner reqWrongCode 403 "code-authentication-failed"

  void $ retryNUntil 6 ((==) 200 . statusCode) $ requestVerificationCode (env ^. teBrig) email Public.CreateScimToken
  code <- getVerificationCode (env ^. teBrig) owner Public.CreateScimToken
  let reqWithCode = CreateScimToken "testCreateToken" (Just defPassword) (Just code) Nothing Nothing
  CreateScimTokenResponseV6 token _ <- createToken owner reqWithCode

  -- Try to do @GET /Users@ and check that it succeeds
  let fltr = filterBy "externalId" "67c196a0-cd0e-11ea-93c7-ef550ee48502"
  listUsers_ (Just token) (Just fltr) (env ^. teSpar)
    !!! const 200 === statusCode
  where
    requestVerificationCode :: BrigReq -> EmailAddress -> Public.VerificationAction -> TestSpar ResponseLBS
    requestVerificationCode brig email action = do
      call $
        post (brig . paths ["verification-code", "send"] . contentJson . json (Public.SendVerificationCode action email))

-- @END

unlockFeature :: GalleyReq -> TeamId -> TestSpar ()
unlockFeature galley tid =
  call $ put (galley . paths ["i", "teams", toByteString' tid, "features", featureNameBS @Public.SndFactorPasswordChallengeConfig, toByteString' Public.LockStatusUnlocked]) !!! const 200 === statusCode

setSndFactorPasswordChallengeStatus :: GalleyReq -> TeamId -> Public.FeatureStatus -> TestSpar ()
setSndFactorPasswordChallengeStatus galley tid status = do
  let js = RequestBodyLBS $ encode $ Public.Feature @Public.SndFactorPasswordChallengeConfig status Public.SndFactorPasswordChallengeConfig
  call $
    put (galley . paths ["i", "teams", toByteString' tid, "features", featureNameBS @Public.SndFactorPasswordChallengeConfig] . contentJson . body js)
      !!! const 200 === statusCode

getVerificationCode :: BrigReq -> UserId -> Public.VerificationAction -> TestSpar Code.Value
getVerificationCode brig uid action = do
  resp <-
    call $
      get (brig . paths ["i", "users", toByteString' uid, "verification-code", toByteString' action])
        <!! const 200 === statusCode
  pure $ responseJsonUnsafe @Code.Value resp

-- | Test that only up to @maxScimTokens@ can be created.
--
-- We assume that in tests, the token limit is 2.
testTokenLimit :: TestSpar ()
testTokenLimit = do
  env <- ask
  -- Create two tokens
  (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  _ <-
    createToken
      owner
      CreateScimToken
        { description = "testTokenLimit / #1",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  _ <-
    createToken
      owner
      CreateScimToken
        { description = "testTokenLimit / #2",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  -- Try to create the third token and see that it fails
  createToken_
    owner
    CreateScimToken
      { description = "testTokenLimit / #3",
        password = Just defPassword,
        verificationCode = Nothing,
        name = Nothing,
        idp = Nothing
      }
    (env ^. teSpar)
    !!! checkErr 403 (Just "token-limit-reached")

testNumIdPs :: TestSpar ()
testNumIdPs = do
  env <- ask
  (owner, _) <-
    runHttpT (env ^. teMgr) $
      createUserWithTeam (env ^. teBrig) (env ^. teGalley)

  let addSomeIdP :: TestSpar ()
      addSomeIdP = do
        let spar = env ^. teSpar
            apiversion = env ^. teWireIdPAPIVersion
        SAML.SampleIdP metadata _ _ _ <- SAML.makeSampleIdPMetadata
        void $ call $ Util.callIdpCreate apiversion spar (Just owner) metadata

  createToken owner (CreateScimToken "eins" (Just defPassword) Nothing Nothing Nothing)
    >>= deleteToken owner . (.stiId) . (.info)
  addSomeIdP
  createToken owner (CreateScimToken "zwei" (Just defPassword) Nothing Nothing Nothing)
    >>= deleteToken owner . (.stiId) . (.info)
  addSomeIdP
  createToken_ owner (CreateScimToken "drei" (Just defPassword) Nothing Nothing Nothing) (env ^. teSpar)
    !!! checkErr 400 (Just "more-than-one-idp")

-- @SF.Provisioning @TSFI.RESTfulAPI @S2
-- Test that a token can only be created as a team owner
testCreateTokenAuthorizesOnlyAdmins :: TestSpar ()
testCreateTokenAuthorizesOnlyAdmins = do
  env <- ask
  (owner, teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner

  let mkUser :: Role -> TestSpar UserId
      mkUser role = do
        runHttpT (env ^. teMgr) $
          createTeamMember
            (env ^. teBrig)
            (env ^. teGalley)
            teamId
            (rolePermissions role)

  let createToken' uid =
        createToken_
          uid
          CreateScimToken
            { description = "testCreateToken",
              password = Just defPassword,
              verificationCode = Nothing,
              name = Nothing,
              idp = Nothing
            }
          (env ^. teSpar)

  (mkUser RoleMember >>= createToken')
    !!! checkErr 403 (Just "insufficient-permissions")

  (mkUser RoleAdmin >>= createToken')
    !!! const 200 === statusCode

-- @END

-- | Test that for a user with a password, token creation requires reauthentication (i.e. the
-- field @"password"@ should be provided).
--
-- Checks both the "password not provided" and "wrong password is provided" cases.
testCreateTokenRequiresPassword :: TestSpar ()
testCreateTokenRequiresPassword = do
  env <- ask
  -- Create a new team
  (owner, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  -- Creating a token doesn't work without a password
  createToken_
    owner
    CreateScimToken
      { description = "testCreateTokenRequiresPassword",
        password = Nothing,
        verificationCode = Nothing,
        name = Nothing,
        idp = Nothing
      }
    (env ^. teSpar)
    !!! checkErr 403 (Just "access-denied")
  -- Creating a token doesn't work with a wrong password
  createToken_
    owner
    CreateScimToken
      { description = "testCreateTokenRequiresPassword",
        password = Just (plainTextPassword6Unsafe "wrong password"),
        verificationCode = Nothing,
        name = Nothing,
        idp = Nothing
      }
    (env ^. teSpar)
    !!! checkErr 403 (Just "access-denied")

----------------------------------------------------------------------------
-- Token listing

-- | Tests for @GET /auth-tokens@.
specListTokens :: SpecWith TestEnv
specListTokens = describe "GET /auth-tokens" $ do
  it "works" $ testListTokens
  it "converts legacy plaintext tokens" $ testPlaintextTokensAreConverted

-- | Test that existing tokens can be listed.
testListTokens :: TestSpar ()
testListTokens = do
  -- Create two tokens
  env <- ask
  (owner, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  _ <-
    createToken
      owner
      CreateScimToken
        { description = "testListTokens / #1",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  _ <-
    createToken
      owner
      CreateScimToken
        { description = "testListTokens / #2",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  -- Check that the token is on the list
  list <- (.scimTokenListTokens) <$> listTokens owner
  liftIO $
    map (.stiDescr) list
      `shouldBe` ["testListTokens / #1", "testListTokens / #2"]

testPlaintextTokensAreConverted :: TestSpar ()
testPlaintextTokensAreConverted = do
  env <- ask
  (owner, teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner

  -- create a legacy plaintext token in the DB
  token <- createLegacyPlaintextToken teamId
  countTokensInDB (ScimTokenLookupKeyPlaintext token) >>= liftIO . (`shouldBe` 1)

  -- scim token can be used.
  -- This use causes the plaintext token to be converted to a hashed version
  void $ listUsers token (Just (filterBy "userName" $ "foo"))

  -- The previous use of the token causes its DB entry to be hashed
  -- The plaintext entry is gone
  countTokensInDB (ScimTokenLookupKeyPlaintext token) >>= liftIO . (`shouldBe` 0)

  -- The hashed entry can be found
  let hashedToken = hashScimToken token
  countTokensInDB (ScimTokenLookupKeyHashed hashedToken) >>= liftIO . (`shouldBe` 1)

  -- token can still be used
  void $ listUsers token (Just (filterBy "userName" $ "foo"))
  where
    wrapMonadClient :: Cas.Client a -> TestSpar a
    wrapMonadClient action = do
      env <- ask
      let clientState = env ^. teCql
      runClient clientState action

    createLegacyPlaintextToken :: TeamId -> TestSpar ScimToken
    createLegacyPlaintextToken teamId = do
      token <- ScimToken . cs . ES.encode <$> liftIO (randBytes 32)
      tokenId <- randomId
      now <- liftIO $ getCurrentTime
      let descr = "legacy test token"
      wrapMonadClient $ do
        retry x5 . batch $ do
          setType BatchLogged
          setConsistency LocalQuorum
          addPrepQuery insByToken (ScimTokenLookupKeyPlaintext token, teamId, tokenId, now, Nothing, descr)
          addPrepQuery insByTeam (ScimTokenLookupKeyPlaintext token, teamId, tokenId, now, Nothing, descr)
      pure token

    insByToken, insByTeam :: PrepQuery W (ScimTokenLookupKey, TeamId, ScimTokenId, UTCTime, Maybe SAML.IdPId, Text) ()
    insByToken =
      [r|
      INSERT INTO team_provisioning_by_token
        (token_, team, id, created_at, idp, descr)
        VALUES (?, ?, ?, ?, ?, ?)
      |]
    insByTeam =
      [r|
      INSERT INTO team_provisioning_by_team
        (token_, team, id, created_at, idp, descr)
        VALUES (?, ?, ?, ?, ?, ?)
      |]

    countTokensInDB :: ScimTokenLookupKey -> TestSpar Int64
    countTokensInDB key =
      wrapMonadClient $ do
        count <- runIdentity <$$> (retry x1 . query1 selByKey $ params LocalQuorum (Identity key))
        pure $ fromMaybe 0 count

    selByKey :: PrepQuery R (Identity ScimTokenLookupKey) (Identity Int64)
    selByKey =
      [r|
      SELECT COUNT(*) FROM team_provisioning_by_token WHERE token_ = ?
      |]

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
  (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  CreateScimTokenResponseV6 token tokenInfo <-
    createToken
      owner
      CreateScimToken
        { description = "testDeletedTokensAreUnusable",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  -- An operation with the token should succeed
  let fltr = filterBy "externalId" "67c196a0-cd0e-11ea-93c7-ef550ee48502"
  listUsers_ (Just token) (Just fltr) (env ^. teSpar)
    !!! const 200 === statusCode
  -- Delete the token and now the operation should fail
  deleteToken owner tokenInfo.stiId
  listUsers_ (Just token) Nothing (env ^. teSpar)
    !!! checkErr 401 Nothing

-- | Test that when a token is deleted, it no longer appears in the result of @GET
-- /auth-tokens@.
testDeletedTokensAreUnlistable :: TestSpar ()
testDeletedTokensAreUnlistable = do
  -- Create a token
  env <- ask
  (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
  _ <- registerTestIdP owner
  CreateScimTokenResponseV6 _ tokenInfo <-
    createToken
      owner
      CreateScimToken
        { description = "testDeletedTokensAreUnlistable",
          password = Just defPassword,
          verificationCode = Nothing,
          name = Nothing,
          idp = Nothing
        }
  -- Delete the token
  deleteToken owner tokenInfo.stiId
  -- Check that the token is not on the list
  list <- (.scimTokenListTokens) <$> listTokens owner
  liftIO $ list `shouldBe` []

----------------------------------------------------------------------------
-- Miscellaneous tests

-- @SF.Provisioning @TSFI.RESTfulAPI @S2
-- This test verifies that the SCIM API responds with an authentication error
-- and can't be used if it receives an invalid secret token
-- or if no token is provided at all
testAuthIsNeeded :: TestSpar ()
testAuthIsNeeded = do
  env <- ask
  -- Try to do @GET /Users@ with an invalid token and check that it fails
  let invalidToken = ScimToken "this-is-an-invalid-token"
  listUsers_ (Just invalidToken) Nothing (env ^. teSpar) !!! checkErr 401 Nothing
  -- Try to do @GET /Users@ without a token and check that it fails
  listUsers_ Nothing Nothing (env ^. teSpar) !!! checkErr 401 Nothing

-- @END
