module Test.OAuth where

import API.Brig
import API.BrigInternal
import Data.String.Conversions
import Network.HTTP.Types
import Network.URI
import SetupHelpers
import Testlib.Prelude

testListApplicationsWithActiveSessions :: (HasCallStack) => App ()
testListApplicationsWithActiveSessions = do
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
  sessions <- app %. "sessions" >>= asList
  length sessions `shouldMatchInt` 2
