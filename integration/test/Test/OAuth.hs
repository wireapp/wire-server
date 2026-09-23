-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.OAuth where

import API.Brig
import API.BrigInternal
import API.Common (defPassword)
import API.Galley
import qualified API.Nginz as Nginz
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as B64U
import Data.String.Conversions
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.CQL.IO
import Network.HTTP.Types
import Network.URI
import SetupHelpers
import Testlib.Prelude

testOAuthRevokeSession :: (HasCallStack) => App ()
testOAuthRevokeSession = do
  user <- randomUser OwnDomain def
  let uri = "https://example.com"
  cid <- createOAuthClient user "foobar" uri >>= getJSON 200 >>= flip (%.) "client_id"
  let scopes = ["write-only:conversations"]

  -- create a session that will be revoked later
  (tokenToBeRevoked, sessionToBeRevoked) <- do
    token <- generateAccessToken user cid scopes uri
    [app] <- getOAuthApplications user >>= getJSON 200 >>= asList
    [session] <- app %. "sessions" >>= asList
    pure (token, session)

  -- create another session and assert that there are two sessions
  validToken <- do
    token <- generateAccessToken user cid scopes uri
    [app] <- getOAuthApplications user >>= getJSON 200 >>= asList
    sessions <- app %. "sessions" >>= asList
    length sessions `shouldMatchInt` 2
    pure token

  -- attempt to revoke a session with a wrong password should fail
  sessionToBeRevoked
    %. "refresh_token_id"
    >>= asString
    >>= deleteOAuthSession user cid "foobar"
    >>= assertStatus 403

  -- revoke the first session and assert that there is only one session left
  sessionToBeRevoked
    %. "refresh_token_id"
    >>= asString
    >>= deleteOAuthSession user cid defPassword
    >>= assertSuccess
  [app] <- getOAuthApplications user >>= getJSON 200 >>= asList
  sessions <- app %. "sessions" >>= asList
  length sessions `shouldMatchInt` 1

  -- try to use the revoked token and assert that it fails
  tokenToBeRevoked
    %. "refresh_token"
    >>= asString
    >>= createOAuthAccessTokenWithRefreshToken user cid
    >>= assertStatus 403

  -- try to use the valid token and assert that it works
  validToken
    %. "refresh_token"
    >>= asString
    >>= createOAuthAccessTokenWithRefreshToken user cid
    >>= assertSuccess

testRevokeApplicationAccountAccessV6 :: App ()
testRevokeApplicationAccountAccessV6 = do
  user <- randomUser OwnDomain def
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 0
  let uri = "https://example.com"
  let scopes = ["write-only:conversations"]
  replicateM_ 3 $ do
    cid <- createOAuthClient user "foobar" uri >>= getJSON 200 >>= flip (%.) "client_id"
    generateAccessToken user cid scopes uri
  [cid1, cid2, cid3] <- getOAuthApplications user >>= getJSON 200 >>= asList >>= mapM (%. "id")
  revokeApplicationAccessV6 user cid1 >>= assertSuccess
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 2
    ids <- for apps $ \app -> app %. "id"
    ids `shouldMatchSet` [cid2, cid3]
  revokeApplicationAccessV6 user cid2 >>= assertSuccess
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 1
    ids <- for apps $ \app -> app %. "id"
    ids `shouldMatchSet` [cid3]
  revokeApplicationAccessV6 user cid3 >>= assertSuccess
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 0

testRevokeApplicationAccountAccess :: App ()
testRevokeApplicationAccountAccess = do
  user <- randomUser OwnDomain def
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 0
  let uri = "https://example.com"
  let scopes = ["write-only:conversations"]
  replicateM_ 3 $ do
    cid <- createOAuthClient user "foobar" uri >>= getJSON 200 >>= flip (%.) "client_id"
    generateAccessToken user cid scopes uri
  [cid1, cid2, cid3] <- getOAuthApplications user >>= getJSON 200 >>= asList >>= mapM (%. "id")
  revokeApplicationAccess user cid1 "foobar" >>= assertStatus 403
  revokeApplicationAccess user cid1 defPassword >>= assertSuccess
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 2
    ids <- for apps $ \app -> app %. "id"
    ids `shouldMatchSet` [cid2, cid3]
  revokeApplicationAccess user cid2 defPassword >>= assertSuccess
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 1
    ids <- for apps $ \app -> app %. "id"
    ids `shouldMatchSet` [cid3]
  revokeApplicationAccess user cid3 defPassword >>= assertSuccess
  bindResponse (getOAuthApplications user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    apps <- resp.json & asList
    length apps `shouldMatchInt` 0

-- | The tiers of a scope are separate: a token that may write may not read,
-- and the other way round.  This is about @/conversations/:cnv/code@, the one
-- location in the integration nginx.conf that uses 'oauth_scopes'.
testOAuthScopeTiersAreSeparate :: (HasCallStack) => App ()
testOAuthScopeTiersAreSeparate = do
  (user, _, _) <- createTeam OwnDomain 1
  conv <- postConversation user (allowGuests defProteus) >>= getJSON 201
  -- with a zauth token, so that there is a code to read later on
  postConversationCode user conv Nothing Nothing >>= assertSuccess

  cid <- oauthClient user
  readToken <- accessTokenFor user cid ["read:conversations_code"]
  writeToken <- accessTokenFor user cid ["write-only:conversations_code"]

  Nginz.getConversationCode user conv readToken >>= assertStatus 200
  Nginz.postConversationCode user conv readToken >>= assertStatus 403

  Nginz.postConversationCode user conv writeToken >>= assertSuccess
  Nginz.getConversationCode user conv writeToken >>= assertStatus 403

testOAuthRejectUnusefulTokenRequests :: (HasCallStack) => App ()
testOAuthRejectUnusefulTokenRequests = do
  (user, _, _) <- createTeam OwnDomain 1
  cid <- oauthClient user

  generateOAuthAuthorizationCode user cid [] redirectUri >>= assertStatus 400
  -- the 400 has to say which scopes would have worked
  bindResponse (generateOAuthAuthorizationCode user cid ["pizza"] redirectUri) $ \resp -> do
    resp.status `shouldMatchInt` 400
    msg <- resp.json %. "message" & asString
    msg `shouldContainString` "read:self"
    msg `shouldContainString` "write-only:conversations"
  generateOAuthAuthorizationCode user cid ["delete-only:conversations_code"] redirectUri >>= assertStatus 400

testOAuthNewScopesOnDeprecatedAttribute :: (HasCallStack) => App ()
testOAuthNewScopesOnDeprecatedAttribute = do
  user <- randomUser OwnDomain def
  cid <- oauthClient user

  selfToken <- accessTokenFor user cid ["read:self"]
  Nginz.getSelf user selfToken >>= assertStatus 200

  convToken <- accessTokenFor user cid ["write-only:conversations"]
  Nginz.postConversation user defProteus convToken >>= assertStatus 201

  -- ... and the deprecated attribute still tells the scopes apart
  Nginz.getSelf user convToken >>= assertStatus 403

testOAuthDeprecatedScopesInCassandra :: (HasCallStack) => TaggedBool "old scope syntax" -> App ()
testOAuthDeprecatedScopesInCassandra (TaggedBool oldScopeSyntax) = do
  (user, _, _) <- createTeam OwnDomain 1
  conv <- postConversation user (allowGuests defProteus) >>= getJSON 201
  postConversationCode user conv Nothing Nothing >>= assertSuccess

  cid <- oauthClient user
  session <- generateAccessToken user cid ["write-only:conversations_code"] redirectUri

  when oldScopeSyntax (hackCassandra user)

  refreshToken <- session %. "refresh_token" & asString
  refreshed <- createOAuthAccessTokenWithRefreshToken user cid refreshToken >>= getJSON 200

  if oldScopeSyntax
    then do
      token <- refreshed %. "access_token" & asString
      hasScopes token ["read:conversations_code", "write-only:conversations_code"]
      Nginz.getConversationCode user conv token >>= assertSuccess
      Nginz.postConversationCode user conv token >>= assertSuccess
    else do
      token <- refreshed %. "access_token" & asString
      hasScopes token ["write-only:conversations_code"]
      Nginz.getConversationCode user conv token >>= assertStatus 403
      Nginz.postConversationCode user conv token >>= assertSuccess
  where
    -- pretend the session was created before the split
    hackCassandra :: Value -> App ()
    hackCassandra user = do
      keyspace <- readServiceConfig Brig & (%. "cassandra.keyspace") & asString
      let setScope :: PrepQuery W (Identity UUID) () =
            fromString
              $ "UPDATE "
              <> keyspace
              <> ".oauth_refresh_token SET scope = {'write:conversations_code', 'read:pizza'} WHERE id = ?"
      rid <- refreshTokenId user
      write setScope (defQueryParams LocalQuorum (Identity rid))

    hasScopes :: String -> [String] -> App ()
    hasScopes token expectedScopes = do
      claims <- accessTokenClaims token
      scopes <- claims %. "scope" & asString
      words scopes `shouldMatchSet` expectedScopes

--------------------------------------------------------------------------------
-- helpers

redirectUri :: String
redirectUri = "https://example.com"

oauthClient :: (HasCallStack, MakesValue user) => user -> App Value
oauthClient user =
  createOAuthClient user "foobar" redirectUri >>= getJSON 200 >>= (%. "client_id")

-- | The access token, which is the part nginz gets to see.
accessTokenFor :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> [String] -> App String
accessTokenFor user cid scopes =
  generateAccessToken user cid scopes redirectUri >>= (%. "access_token") >>= asString

-- | The id of the one session the user has.
refreshTokenId :: (HasCallStack, MakesValue user) => user -> App UUID
refreshTokenId user = do
  [app] <- getOAuthApplications user >>= getJSON 200 >>= asList
  [session] <- app %. "sessions" >>= asList
  rid <- session %. "refresh_token_id" & asString
  maybe (assertFailure ("not a uuid: " <> rid)) pure (UUID.fromString rid)

-- | The claims of an access token, read without verifying anything: we only
-- want to see what brig put in.
accessTokenClaims :: (HasCallStack) => String -> App Value
accessTokenClaims token = do
  payload <- case T.splitOn (cs ".") (cs token) of
    (_ : p : _) -> pure p
    _ -> assertFailure ("not a JWT: " <> token)
  claims <- case B64U.decodeUnpadded (cs payload) of
    Left e -> assertFailure ("not base64url: " <> token <> ": " <> e)
    Right bs -> pure bs
  case Aeson.eitherDecode (cs claims) of
    Left e -> assertFailure ("not json: " <> token <> ": " <> e)
    Right v -> pure v

generateAccessToken :: (MakesValue cid, MakesValue user) => user -> cid -> [String] -> String -> App Value
generateAccessToken user cid scopes uri = do
  authCodeResponse <- generateOAuthAuthorizationCode user cid scopes uri
  let location = fromMaybe (error "no location header") $ parseURI . cs . snd =<< locationHeader authCodeResponse
  let code = maybe "no code query param" cs $ join $ lookup (cs "code") $ parseQuery $ cs location.uriQuery
  createOAuthAccessToken user cid code uri >>= getJSON 200
