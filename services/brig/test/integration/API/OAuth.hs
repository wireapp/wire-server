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

module API.OAuth where

import Bilge
import Bilge.Assert
import Brig.API.OAuth
import Brig.Effects.Jwk (readJwk)
import Brig.Options
import qualified Brig.Options as Opt
import Control.Lens
import Crypto.JOSE (JWK)
import Crypto.JWT (Audience (Audience), NumericDate (NumericDate), claimAud, claimExp, claimIat, claimIss, claimSub, stringOrUri)
import qualified Data.Aeson as A
import Data.ByteString.Conversion (fromByteString, fromByteString', toByteString')
import Data.Id (OAuthClientId, UserId, idToText, randomId)
import Data.Range (unsafeRange)
import Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text.Ascii (encodeBase16)
import Data.Time
import Imports
import qualified Network.Wai.Utilities as Error
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString
import Util
import Web.FormUrlEncoded
import Wire.API.User

tests :: Manager -> Brig -> Opts -> TestTree
tests m b opts = do
  testGroup "oauth" $
    [ test m "register new OAuth client" $ testRegisterNewOAuthClient b,
      testGroup "create oauth code" $
        [ test m "success" $ testCreateOAuthCodeSuccess b,
          test m "oauth client not found" $ testCreateOAuthCodeClientNotFound b,
          test m "redirect url mismatch" $ testCreateOAuthCodeRedirectUrlMismatch b
        ],
      testGroup "create access token" $
        [ test m "success" $ testCreateAccessTokenSuccess opts b,
          test m "wrong client id fail" $ testCreateAccessTokenWrongClientId b,
          test m "wrong client secret fail" $ testCreateAccessTokenWrongClientSecret b,
          test m "wrong code fail" $ testCreateAccessTokenWrongAuthCode b,
          test m "wrong redirect url fail" $ testCreateAccessTokenWrongUrl b
        ]
    ]

testRegisterNewOAuthClient :: Brig -> Http ()
testRegisterNewOAuthClient brig = do
  let newOAuthClient@(NewOAuthClient expectedAppName expectedUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- userId <$> randomUser brig
  oauthClientInfo <- getOAuthClientInfo brig uid cid
  liftIO $ do
    expectedAppName @?= ocName oauthClientInfo
    expectedUrl @?= ocRedirectUrl oauthClientInfo

testCreateOAuthCodeSuccess :: Brig -> Http ()
testCreateOAuthCodeSuccess brig = do
  let newOAuthClient@(NewOAuthClient _ redirectUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- userId <$> randomUser brig
  let scope = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  let state = "foobar"
  createOAuthCode brig uid (NewOAuthAuthCode cid scope OAuthResponseTypeCode redirectUrl state) !!! do
    const 302 === statusCode
    const (Just $ unRedirectUrl redirectUrl ^. pathL) === (fmap getPath . getLocation)
    const (Just $ ["code", "state"]) === (fmap (fmap fst . getQueryParams) . getLocation)
    const (Just $ cs state) === (getLocation >=> getQueryParamValue "state")
  where
    getLocation :: ResponseLBS -> Maybe RedirectUrl
    getLocation = getHeader "Location" >=> fromByteString

    getPath :: RedirectUrl -> ByteString
    getPath (RedirectUrl uri) = uri ^. pathL

    getQueryParams :: RedirectUrl -> [(ByteString, ByteString)]
    getQueryParams (RedirectUrl uri) = uri ^. (queryL . queryPairsL)

    getQueryParamValue :: ByteString -> RedirectUrl -> Maybe ByteString
    getQueryParamValue key uri = snd <$> find ((== key) . fst) (getQueryParams uri)

testCreateOAuthCodeRedirectUrlMismatch :: Brig -> Http ()
testCreateOAuthCodeRedirectUrlMismatch brig = do
  cid <- occClientId <$> registerNewOAuthClient brig (newOAuthClientRequestBody "E Corp" "https://example.com")
  uid <- userId <$> randomUser brig
  let differentUrl = fromMaybe (error "invalid url") $ fromByteString' "https://wire.com"
  createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode differentUrl "") !!! do
    const 400 === statusCode
    const (Just "redirect-url-miss-match") === fmap Error.label . responseJsonMaybe

testCreateOAuthCodeClientNotFound :: Brig -> Http ()
testCreateOAuthCodeClientNotFound brig = do
  cid <- randomId
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode redirectUrl "") !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenSuccess :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenSuccess opts brig = do
  now <- liftIO getCurrentTime
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  accessToken <- createOAuthAccessToken brig accessTokenRequest
  -- authorization code should be deleted and can only be used once
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe
  k <- liftIO $ readJwk (fromMaybe "" (Opt.setOAuthJwkKeyPair $ Opt.optSettings opts)) <&> fromMaybe (error "invalid key")
  verifiedOrError <- liftIO $ verify k (cs $ unOauthAccessToken $ oatAccessToken accessToken)
  verifiedOrErrorWithWrongKey <- liftIO $ verify wrongKey (cs $ unOauthAccessToken $ oatAccessToken accessToken)
  liftIO $ do
    isRight verifiedOrError @?= True
    isLeft verifiedOrErrorWithWrongKey @?= True
    let claims = either (error "invalid token") id verifiedOrError
    scope claims @?= scopes
    (view claimIss $ claims) @?= ("example.com" ^? stringOrUri @Text)
    (view claimAud $ claims) @?= (Audience . (: []) <$> "example.com" ^? stringOrUri @Text)
    (view claimSub $ claims) @?= (idToText uid ^? stringOrUri)
    let expTime = (\(NumericDate x) -> x) . fromMaybe (error "exp claim missing") . view claimExp $ claims
    diffUTCTime expTime now > 0 @?= True
    let issuingTime = (\(NumericDate x) -> x) . fromMaybe (error "iat claim missing") . view claimIat $ claims
    abs (diffUTCTime issuingTime now) < 5 @?= True -- allow for some generous clock skew

testCreateAccessTokenWrongClientId :: Brig -> Http ()
testCreateAccessTokenWrongClientId brig = do
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (_, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  cid <- randomId
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongClientSecret :: Brig -> Http ()
testCreateAccessTokenWrongClientSecret brig = do
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, _, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let secret = OAuthClientPlainTextSecret $ encodeBase16 "ee2316e304f5c318e4607d86748018eb9c66dc4f391c31bcccd9291d24b4c7e"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongAuthCode :: Brig -> Http ()
testCreateAccessTokenWrongAuthCode brig = do
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, secret, _) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let code = OAuthAuthCode $ encodeBase16 "eb32eb9e2aa36c081c89067dddf81bce83c1c57e0b74cfb14c9f026f145f2b1f"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongUrl :: Brig -> Http ()
testCreateAccessTokenWrongUrl brig = do
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://wire.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let wrongUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code wrongUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

-------------------------------------------------------------------------------
-- Util

newOAuthClientRequestBody :: Text -> Text -> NewOAuthClient
newOAuthClientRequestBody name url =
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' (cs url)
      applicationName = OAuthApplicationName (unsafeRange name)
   in NewOAuthClient applicationName redirectUrl

registerNewOAuthClient :: HasCallStack => Brig -> NewOAuthClient -> Http OAuthClientCredentials
registerNewOAuthClient brig reqBody =
  responseJsonError =<< post (brig . paths ["i", "oauth", "clients"] . json reqBody) <!! const 200 === statusCode

getOAuthClientInfo :: HasCallStack => Brig -> UserId -> OAuthClientId -> Http OAuthClient
getOAuthClientInfo brig uid cid =
  responseJsonError =<< get (brig . paths ["oauth", "clients", toByteString' cid] . zUser uid) <!! const 200 === statusCode

createOAuthCode :: HasCallStack => Brig -> UserId -> NewOAuthAuthCode -> Http ResponseLBS
createOAuthCode brig uid reqBody = post (brig . paths ["oauth", "authorization", "codes"] . zUser uid . json reqBody . noRedirect)

createOAuthAccessToken :: HasCallStack => Brig -> OAuthAccessTokenRequest -> Http OAuthAccessTokenResponse
createOAuthAccessToken brig reqBody = responseJsonError =<< createOAuthAccessToken' brig reqBody

createOAuthAccessToken' :: HasCallStack => Brig -> OAuthAccessTokenRequest -> Http ResponseLBS
createOAuthAccessToken' brig reqBody = do
  post (brig . paths ["oauth", "token"] . content "application/x-www-form-urlencoded" . body (RequestBodyLBS $ urlEncodeAsForm reqBody))

generateOAuthClientAndAuthCode :: Brig -> UserId -> OAuthScopes -> RedirectUrl -> Http (OAuthClientId, OAuthClientPlainTextSecret, OAuthAuthCode)
generateOAuthClientAndAuthCode brig uid scope url = do
  let newOAuthClient = NewOAuthClient (OAuthApplicationName (unsafeRange "E Corp")) url
  OAuthClientCredentials cid secret <- registerNewOAuthClient brig newOAuthClient
  let state = "foobar"
  response <-
    createOAuthCode brig uid (NewOAuthAuthCode cid scope OAuthResponseTypeCode url state) <!! do
      const 302 === statusCode
  maybe (error "generating code failed") (pure . (,,) cid secret) $ (getHeader "Location" >=> fromByteString >=> getQueryParamValue "code" >=> fromByteString) response
  where
    getQueryParams :: RedirectUrl -> [(ByteString, ByteString)]
    getQueryParams (RedirectUrl uri) = uri ^. (queryL . queryPairsL)

    getQueryParamValue :: ByteString -> RedirectUrl -> Maybe ByteString
    getQueryParamValue key uri = snd <$> find ((== key) . fst) (getQueryParams uri)

wrongKey :: JWK
wrongKey = fromMaybe (error "invalid jwk") $ A.decode "{\"p\":\"-Ahl1aNMOqXLUtJHVO1OLGt92EOrjzcNlwB5AL9hp8-GykJIK6BIfDvCCJgDUX-8ZZ-1R485XFVtUiI5W72MKbJ-qicTB7Smzd7St_zO6PZUbkgQoJiosAOMjP_8DBs9CbMl9FqUfE1pNo4O0gYHslUoCKwS5IsAB9HjuHGEQ38\",\"kty\":\"RSA\",\"q\":\"qRih0wBK2xg2wyJcBN6dDpUHTBxNEt8jxmvy33oMU-_Vx0hFLVeAqDYK-awlHGtJQJKp1mXdURXocKXKPukVitnfEH8nvl6vQIr4-uXyENe3yLgADi8VRDZCbWuDVWYAlYlFgdNODZ_A_fIqCmGAw27bwXyZZ3IRusnipyFN6iM\",\"d\":\"L0uBKJrI4I-_X9KPQawrLDEnPT7msevOH5Rf264CPZgwe8B9M0mbGmhIzYFIThNSaEzGoEtyJdTf27zoawh3O3KQO0aJr2HKSCTMZUh7fpqIjYlu5jA_dT3k7yHHMIR4lRLQV0vb936Mu09kTkRqMZ0jSo46dJ5iw0wnuSF0dAiqVG0rSJK-gVBdIbzZYxhSBW4ZF3n4CqtFb6lc1stfZHcnzWHyF6Cofzup6pJumeFe7xXF9-aGU-3UcTSzTnMa21NVP-vT2CXkH8dSfwLI-PuJwlW6tcpBwT2PXrCGyAGqQ3h5cdAmwcgfbla8wqrzj1A08SlkKHvTDixVvnnzpQ\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"0makAydOdX3vNv4YTToO45ccQUCOoLisvAFVyhiKA4c\",\"qi\":\"phNbA_tiDLQq1omVgM1dHtOe6Dd7J_ZoRdz1Rmc4uaSQyJe-yn88DxXlX10DJkM9uqyzcojOtD5awBUXgYSzmasZvcZ0e2XNi7iXmSwsggTux3lUVVqKWV8HreaSywJ-HqitxjitooWSWOyD9o8yq9RS4r2QdXyuCfthwnEZdpc\",\"dp\":\"q0IJJmjZYolFiYsdq5sq5erWerPGyl0l6gRuiECcqiTVmeQINu81_Wm5gPuNFwHO0JBkt-NBpOprUFHHLvwCwmu3n77ZGfH3VqCq-FT7fMlQ5NCngmvF1bqtmlHJ84X_MCpdY4oDioxcwEl4HDYDrHO17774UItVWxDmXl0rCPs\",\"alg\":\"PS512\",\"dq\":\"NznQQDVsPTofSIPEQeLisIyDoZvsoCk4ael_nPUjaZZ-32L_FNvrLQTZeMl8JVf0yJ4d0ePa8EyTaZb8AqflXT_i1mRw-n-6BP5earMG5_FMGMXfXsKJ04lVEJ94eT-jGTOH--qjJ1fxk_6vNEy73RgrtXmYMGzU1Yhx-duqsrk\",\"n\":\"o9VozUwUc1mQMrAH2fEna_ihmNa3CVRzK7MUgDHEbfY0T71wREpK4f4fOkDysKIqnmMdxRzJhsXTDpxX8_8AlKcimPgR8Qb2z7GwDsnDZOdgAYrZ7l7gj0nX02IX35MBk7a7tWr0nILFLV9SxEu6UFcZo0bL2Rhck81TRqLbomJpIzAq8VCS8uMQeg6hEMarl9tGvSKyFuMdTCV3JE9dSv_NErAWx7uBIgkai3Imjs4ufatvRsi9ZHaUV5V3NtrFbYDulg-GOH1eXZwnO6UrKgcAdB3nS1WKL-vcxqupceAHeFHRjARm6AV07hJyXVOVHxdffv6BFX5GihFPFvpQXQ\"}"
