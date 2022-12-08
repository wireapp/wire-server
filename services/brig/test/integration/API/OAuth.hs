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
import Brig.Effects.Jwk (fakeJwk)
import Brig.Options
import Control.Lens
import Crypto.JWT (Audience (Audience), NumericDate (NumericDate), claimAud, claimExp, claimIat, claimIss, claimSub, stringOrUri)
import Data.ByteString.Conversion (fromByteString, fromByteString', toByteString')
import Data.Id (OAuthClientId, UserId, idToText, randomId)
import Data.Range (unsafeRange)
import Data.Set as Set
import Data.String.Conversions (cs)
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
tests m b _opts = do
  testGroup "oauth" $
    [ test m "register new OAuth client" $ testRegisterNewOAuthClient b,
      test m "create oauth code - success" $ testCreateOAuthCodeSuccess b,
      test m "create oauth code - oauth client not found" $ testCreateOAuthCodeClientNotFound b,
      test m "create oauth code - redirect url mismatch" $ testCreateOAuthCodeRedirectUrlMismatch b,
      test m "create access token - success" $ testCreateAccessTokenSuccess b
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

testCreateAccessTokenSuccess :: Brig -> Http ()
testCreateAccessTokenSuccess brig = do
  now <- liftIO getCurrentTime
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  accessToken <- createOAuthAccessToken brig accessTokenRequest
  verifiedOrError <- liftIO $ verify (fromMaybe (error "invalid key") fakeJwk) (cs $ unOauthAccessToken $ oatAccessToken accessToken)
  liftIO $ do
    isRight verifiedOrError @?= True
    let claims = either (error "invalid token") id verifiedOrError
    scope claims @?= scopes
    (view claimIss $ claims) @?= ("example.com" ^? stringOrUri @Text)
    (view claimAud $ claims) @?= (Audience . (: []) <$> "example.com" ^? stringOrUri @Text)
    (view claimSub $ claims) @?= (idToText uid ^? stringOrUri)
    let expTime = (\(NumericDate x) -> x) . fromMaybe (error "exp claim missing") . view claimExp $ claims
    diffUTCTime expTime now > 0 @?= True
    let issuingTime = (\(NumericDate x) -> x) . fromMaybe (error "iat claim missing") . view claimIat $ claims
    diffUTCTime issuingTime now < 0 @?= True

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
createOAuthAccessToken brig reqBody = do
  r <- post (brig . paths ["oauth", "token"] . content "application/x-www-form-urlencoded" . body (RequestBodyLBS $ urlEncodeAsForm reqBody))
  responseJsonError r

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
