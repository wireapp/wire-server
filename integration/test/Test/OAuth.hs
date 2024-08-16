module Test.OAuth where

import API.Brig
import API.BrigInternal
import API.Common (defPassword)
import Data.String.Conversions
import Network.HTTP.Types
import Network.URI
import SetupHelpers
import Testlib.Prelude

testOAuthRevokeSession :: (HasCallStack) => App ()
testOAuthRevokeSession = do
  user <- randomUser OwnDomain def
  let uri = "https://example.com"
  cid <- createOAuthClient user "foobar" uri >>= getJSON 200 >>= flip (%.) "client_id"
  let scopes = ["write:conversations"]

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

generateAccessToken :: (MakesValue cid, MakesValue user) => user -> cid -> [String] -> String -> App Value
generateAccessToken user cid scopes uri = do
  authCodeResponse <- generateOAuthAuthorizationCode user cid scopes uri
  let location = fromMaybe (error "no location header") $ parseURI . cs . snd =<< locationHeader authCodeResponse
  let code = maybe "no code query param" cs $ join $ lookup (cs "code") $ parseQuery $ cs location.uriQuery
  createOAuthAccessToken user cid code uri >>= getJSON 200
