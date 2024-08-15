module Test.OAuth where

import API.Brig
import API.BrigInternal
import Data.String.Conversions
import Network.HTTP.Types
import Network.URI
import SetupHelpers
import Testlib.Prelude

testOAuthRevokeRefreshToken :: (HasCallStack) => App ()
testOAuthRevokeRefreshToken = do
  user <- randomUser OwnDomain def
  oauthClient <- createOAuthClient user "foobar" "https://example.com" >>= getJSON 200
  cid <- oauthClient %. "client_id"
  let scopes = ["write:conversations"]
  let generateAccessToken = do
        authCodeResponse <- generateOAuthAuthorizationCode user cid scopes "https://example.com"
        let location = fromMaybe (error "no location header") $ parseURI . cs . snd =<< locationHeader authCodeResponse
        let code = maybe "no code query param" cs $ join $ lookup (cs "code") $ parseQuery $ cs location.uriQuery
        void $ createOAuthAccessToken user cid code "https://example.com" >>= getJSON 200
  replicateM_ 2 generateAccessToken
  [app] <- getOAuthApplications user >>= getJSON 200 >>= asList
  [session1, session2] <- app %. "sessions" >>= asList
  sid1 <- session1 %. "refresh_token_id" >>= asString
  deleteSession user cid sid1 >>= assertSuccess
  [app'] <- getOAuthApplications user >>= getJSON 200 >>= asList
  [session] <- app' %. "sessions" >>= asList
  session %. "refresh_token_id" `shouldMatch` (session2 %. "refresh_token_id")

-- also assert that we cannot get an access token with the revoked refresh token
