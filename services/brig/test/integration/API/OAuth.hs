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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.OAuth where

import Bilge
import Bilge.Assert
import Brig.API.OAuth
import Brig.Effects.Jwk (readJwk)
import Brig.Options
import qualified Brig.Options as Opt
import qualified Cassandra as C
import Control.Lens
import Control.Monad.Catch (MonadCatch)
import Crypto.JOSE (JOSE, JWK, bestJWSAlg, newJWSHeader, runJOSE)
import Crypto.JWT (Audience (Audience), ClaimsSet, JWTError, NumericDate (NumericDate), SignedJWT, claimAud, claimExp, claimIat, claimIss, claimSub, defaultJWTValidationSettings, signJWT, stringOrUri, verifyClaims)
import qualified Data.Aeson as A
import Data.ByteString.Conversion (fromByteString, fromByteString', toByteString')
import Data.Domain (domainText)
import Data.Id
import Data.Range (unsafeRange)
import Data.Set as Set hiding (null, (\\))
import Data.String.Conversions (cs)
import Data.Text.Ascii (encodeBase16)
import qualified Data.Text.Encoding as T
import Data.Time
import Imports
import Network.HTTP.Types (HeaderName)
import qualified Network.Wai.Utilities as Error
import Servant.API
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ
import URI.ByteString
import Util
import Web.FormUrlEncoded
import Wire.API.OAuth
import Wire.API.Routes.Bearer (Bearer (Bearer, unBearer))
import Wire.API.User (SelfProfile, User (userId), userEmail)
import Wire.API.User.Auth (CookieType (PersistentCookie))

tests :: Manager -> C.ClientState -> Brig -> Nginz -> Opts -> TestTree
tests m db b n o = do
  testGroup
    "oauth"
    [ test m "register new oauth client" $ testRegisterNewOAuthClient b,
      testGroup
        "create oauth code"
        [ test m "success" $ testCreateOAuthCodeSuccess b,
          test m "oauth client not found" $ testCreateOAuthCodeClientNotFound b,
          test m "redirect url mismatch" $ testCreateOAuthCodeRedirectUrlMismatch b
        ],
      testGroup
        "create access token"
        [ test m "success" $ testCreateAccessTokenSuccess o b,
          test m "wrong client id fail" $ testCreateAccessTokenWrongClientId b,
          test m "wrong client secret fail" $ testCreateAccessTokenWrongClientSecret b,
          test m "wrong code fail" $ testCreateAccessTokenWrongAuthCode b,
          test m "wrong redirect url fail" $ testCreateAccessTokenWrongUrl b,
          test m "expired code fail" $ testCreateAccessTokenExpiredCode o b
        ],
      testGroup
        "access denied when disabled"
        [ test m "register" $ testRegisterOAuthClientAccessDeniedWhenDisabled o b,
          test m "get client info" $ testGetOAuthClientInfoAccessDeniedWhenDisabled o b,
          test m "create code" $ testCreateCodeOAuthClientAccessDeniedWhenDisabled o b,
          test m "create token" $ testCreateAccessTokenAccessDeniedWhenDisabled o b
        ],
      testGroup
        "accessing a resource"
        [ test m "success (internal)" $ testAccessResourceSuccessInternal b,
          test m "success (nginz)" $ testAccessResourceSuccessNginz b n,
          test m "insufficient scope" $ testAccessResourceInsufficientScope b,
          test m "expired token" $ testAccessResourceExpiredToken o b,
          test m "nonsense token" $ testAccessResourceNonsenseToken b,
          test m "no token" $ testAccessResourceNoToken b,
          test m "invalid signature" $ testAccessResourceInvalidSignature o b
        ],
      testGroup "refresh tokens" $
        [ test m "max active tokens" $ testRefreshTokenMaxActiveTokens o db b
        ]
    ]

testRegisterNewOAuthClient :: Brig -> Http ()
testRegisterNewOAuthClient brig = do
  let newOAuthClient@(NewOAuthClient expectedAppName expectedUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- randomId
  oauthClientInfo <- getOAuthClientInfo brig uid cid
  liftIO $ do
    expectedAppName @?= ocName oauthClientInfo
    expectedUrl @?= ocRedirectUrl oauthClientInfo

testCreateOAuthCodeSuccess :: Brig -> Http ()
testCreateOAuthCodeSuccess brig = do
  let newOAuthClient@(NewOAuthClient _ redirectUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- randomId
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
  uid <- randomId
  let differentUrl = fromMaybe (error "invalid url") $ fromByteString' "https://wire.com"
  createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode differentUrl "") !!! do
    const 400 === statusCode
    const (Just "redirect-url-miss-match") === fmap Error.label . responseJsonMaybe

testCreateOAuthCodeClientNotFound :: Brig -> Http ()
testCreateOAuthCodeClientNotFound brig = do
  cid <- randomId
  uid <- randomId
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode redirectUrl "") !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenSuccess :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenSuccess opts brig = do
  now <- liftIO getCurrentTime
  uid <- userId <$> createUser "alice" brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [SelfRead]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  accessToken <- createOAuthAccessToken brig accessTokenRequest
  -- authorization code should be deleted and can only be used once
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe
  k <- liftIO $ readJwk (fromMaybe "path to jwk not set" (Opt.setOAuthJwkKeyPair $ Opt.optSettings opts)) <&> fromMaybe (error "invalid key")
  verifiedOrError <- liftIO $ verify k (unOAuthToken $ oatAccessToken accessToken)
  verifiedOrErrorWithotherKey <- liftIO $ verify badKey (unOAuthToken $ oatAccessToken accessToken)
  let expectedDomain = domainText $ Opt.setFederationDomain $ Opt.optSettings opts
  liftIO $ do
    isRight verifiedOrError @?= True
    isLeft verifiedOrErrorWithotherKey @?= True
    let claims = either (error "invalid token") id verifiedOrError
    scope claims @?= scopes
    (view claimIss $ claims) @?= (expectedDomain ^? stringOrUri @Text)
    (view claimAud $ claims) @?= (Audience . (: []) <$> expectedDomain ^? stringOrUri @Text)
    (view claimSub $ claims) @?= (idToText uid ^? stringOrUri)
    let expTime = (\(NumericDate x) -> x) . fromMaybe (error "exp claim missing") . view claimExp $ claims
    diffUTCTime expTime now > 0 @?= True
    let issuingTime = (\(NumericDate x) -> x) . fromMaybe (error "iat claim missing") . view claimIat $ claims
    abs (diffUTCTime issuingTime now) < 5 @?= True -- allow for some generous clock skew

testCreateAccessTokenWrongClientId :: Brig -> Http ()
testCreateAccessTokenWrongClientId brig = do
  uid <- randomId
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
  uid <- randomId
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, _, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let secret = OAuthClientPlainTextSecret $ encodeBase16 "ee2316e304f5c318e4607d86748018eb9c66dc4f391c31bcccd9291d24b4c7e"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 403 === statusCode
    const (Just "forbidden") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongAuthCode :: Brig -> Http ()
testCreateAccessTokenWrongAuthCode brig = do
  uid <- randomId
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
  uid <- randomId
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://wire.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let wrongUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code wrongUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 400 === statusCode
    const (Just "redirect-url-miss-match") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenExpiredCode :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenExpiredCode opts brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthAuthCodeExpirationTimeSecsInternal ?~ 1) $ do
    uid <- randomId
    let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
    let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
    (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
    liftIO $ threadDelay (1 * 1200 * 1000)
    let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
    createOAuthAccessToken' brig accessTokenRequest !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe

testGetOAuthClientInfoAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testGetOAuthClientInfoAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthEnabledInternal ?~ False) $ do
    cid <- randomId
    uid <- randomId
    getOAuthClientInfo' brig uid cid !!! assertAccessDenied

testCreateCodeOAuthClientAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testCreateCodeOAuthClientAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthEnabledInternal ?~ False) $ do
    cid <- randomId
    uid <- randomId
    let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
    createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode redirectUrl "") !!! assertAccessDenied

testCreateAccessTokenAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthEnabledInternal ?~ False) $ do
    cid <- randomId
    let secret = OAuthClientPlainTextSecret $ encodeBase16 "ee2316e304f5c318e4607d86748018eb9c66dc4f391c31bcccd9291d24b4c7e"
    let code = OAuthAuthCode $ encodeBase16 "eb32eb9e2aa36c081c89067dddf81bce83c1c57e0b74cfb14c9f026f145f2b1f"
    let wrongUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
    let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code wrongUrl
    createOAuthAccessToken' brig accessTokenRequest !!! assertAccessDenied

testRegisterOAuthClientAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testRegisterOAuthClientAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthEnabledInternal ?~ False) $ do
    let newOAuthClient = newOAuthClientRequestBody "E Corp" "https://example.com"
    registerNewOAuthClient' brig newOAuthClient !!! assertAccessDenied

assertAccessDenied :: Assertions ()
assertAccessDenied = do
  const 403 === statusCode
  const (Just "forbidden") === fmap Error.label . responseJsonMaybe

testAccessResourceSuccessInternal :: Brig -> Http ()
testAccessResourceSuccessInternal brig = do
  uid <- userId <$> createUser "alice" brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [SelfRead]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  accessToken <- createOAuthAccessToken brig accessTokenRequest
  -- should succeed with Z-User header
  response :: SelfProfile <- responseJsonError =<< get (brig . paths ["self"] . zUser uid) <!! const 200 === statusCode
  -- should succeed with Z-OAuth header containing an OAuth bearer token
  response' :: SelfProfile <- responseJsonError =<< get (brig . paths ["self"] . zOAuthHeader (oatAccessToken accessToken)) <!! const 200 === statusCode
  liftIO $ response @?= response'

testAccessResourceSuccessNginz :: Brig -> Nginz -> Http ()
testAccessResourceSuccessNginz brig nginz = do
  -- with ZAuth header
  user <- createUser "alice" brig
  let email = fromMaybe (error "no email") $ userEmail user
  zauthToken <- decodeToken <$> (login nginz (defEmailLogin email) PersistentCookie <!! const 200 === statusCode)
  get (nginz . path "/self" . header "Authorization" ("Bearer " <> toByteString' zauthToken)) !!! const 200 === statusCode

  -- with Authorization header containing an OAuth bearer token
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [SelfRead]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig (userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  oauthToken <- oatAccessToken <$> createOAuthAccessToken brig accessTokenRequest
  get (nginz . paths ["self"] . authHeader oauthToken) !!! const 200 === statusCode

testAccessResourceInsufficientScope :: Brig -> Http ()
testAccessResourceInsufficientScope brig = do
  uid <- userId <$> createUser "alice" brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ConversationCreate]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  accessToken <- createOAuthAccessToken brig accessTokenRequest
  get (brig . paths ["self"] . zOAuthHeader (oatAccessToken accessToken)) !!! do
    const 403 === statusCode
    const "Access denied" === statusMessage
    const (Just "Insufficient scope") === responseBody

testAccessResourceExpiredToken :: Opt.Opts -> Brig -> Http ()
testAccessResourceExpiredToken opts brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthAccessTokenExpirationTimeSecsInternal ?~ 1) $ do
    uid <- userId <$> createUser "alice" brig
    let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
    let scopes = OAuthScopes $ Set.fromList [SelfRead]
    (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
    let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
    accessToken <- createOAuthAccessToken brig accessTokenRequest
    liftIO $ threadDelay (1 * 1200 * 1000)
    get (brig . paths ["self"] . zOAuthHeader (oatAccessToken accessToken)) !!! do
      const 403 === statusCode
      const "Access denied" === statusMessage
      const (Just "Invalid token: JWTExpired") === responseBody

testAccessResourceNonsenseToken :: Brig -> Http ()
testAccessResourceNonsenseToken brig = do
  get (brig . paths ["self"] . zOAuthHeader @Text "foo") !!! do
    const 403 === statusCode
    const "Access denied" === statusMessage
    const (Just "Invalid token: Failed reading: JWSError") =~= responseBody

testAccessResourceNoToken :: Brig -> Http ()
testAccessResourceNoToken brig =
  get (brig . paths ["self"]) !!! do
    const 403 === statusCode
    const "Access denied" === statusMessage

testAccessResourceInvalidSignature :: Opt.Opts -> Brig -> Http ()
testAccessResourceInvalidSignature opts brig = do
  uid <- userId <$> createUser "alice" brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [SelfRead]
  (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
  accessToken <- createOAuthAccessToken brig accessTokenRequest
  key <- liftIO $ readJwk (fromMaybe "path to jwk not set" (Opt.setOAuthJwkKeyPair $ Opt.optSettings opts)) <&> fromMaybe (error "invalid key")
  claimSet <- fromRight (error "token invalid") <$> liftIO (verify key (unOAuthToken $ oatAccessToken accessToken))
  tokenSignedWithotherKey <- signJwtToken badKey claimSet
  get (brig . paths ["self"] . zOAuthHeader (OAuthToken tokenSignedWithotherKey)) !!! do
    const 403 === statusCode
    const "Access denied" === statusMessage
    const (Just "Invalid token: JWSError JWSInvalidSignature") === responseBody

testRefreshTokenMaxActiveTokens :: Opts -> C.ClientState -> Brig -> Http ()
testRefreshTokenMaxActiveTokens opts db brig =
  withSettingsOverrides (opts & Opt.optionSettings . Opt.oauthMaxActiveRefreshTokensInternal ?~ 2) $ do
    uid <- randomId
    jwk <- liftIO $ readJwk (fromMaybe "path to jwk not set" (Opt.setOAuthJwkKeyPair $ Opt.optSettings opts)) <&> fromMaybe (error "invalid key")
    let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
    let scopes = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
    let delayOneSec =
          -- we have to wait ~1 sec before we create the next token, to make sure it is created with a different timestamp
          -- this is due to the interpreter of the `Now` effect which auto-updates every second
          -- FUTUREWORK: once the interpreter of the `Now` effect is changed to use a monotonic clock, we can remove this delay
          threadDelay $ 1000 * 1000
    (rid1, cid, secret) <- do
      let testMsg = "0 active refresh tokens - 1st requested token will be active"
      (cid, secret, code) <- generateOAuthClientAndAuthCode brig uid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
      rt <- oatRefreshToken <$> createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk rt
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid] `hasSameElems` (oriId <$> tokens)
      pure (rid, cid, secret)
    delayOneSec
    rid2 <- do
      let testMsg = "1 active refresh token - 2nd requested token will added to active tokens"
      code <- generateOAuthAuthCode brig uid cid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
      rt <- oatRefreshToken <$> createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk rt
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid1, rid] `hasSameElems` (oriId <$> tokens)
      pure rid
    delayOneSec
    rid3 <- do
      let testMsg = "2 active refresh tokens - 3rd token requested replaces the 1st one"
      code <- generateOAuthAuthCode brig uid cid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
      rt <- oatRefreshToken <$> createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk rt
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid2, rid] `hasSameElems` (oriId <$> tokens)
      pure rid
    delayOneSec
    do
      let testMsg = "2 active refresh tokens - 4th token requests replaces the 2nd one"
      code <- generateOAuthAuthCode brig uid cid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid secret code redirectUrl
      rt <- oatRefreshToken <$> createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk rt
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid3, rid] `hasSameElems` (oriId <$> tokens)
  where
    extractRefreshTokenId :: MonadIO m => JWK -> OAuthRefreshToken -> m OAuthRefreshTokenId
    extractRefreshTokenId jwk rt = do
      fromMaybe (error "invalid sub") . hcsSub <$> liftIO (verifyRefreshToken jwk (unOAuthToken rt))

    hasSameElems :: (Eq a) => [a] -> [a] -> Bool
    hasSameElems x y = null (x \\ y) && null (y \\ x)

    verifyRefreshToken :: JWK -> SignedJWT -> IO ClaimsSet
    verifyRefreshToken jwk jwt =
      fromRight (error "invalid jwt or jwk")
        <$> runJOSE (verifyClaims (defaultJWTValidationSettings (const True)) jwk jwt :: JOSE JWTError IO ClaimsSet)

-------------------------------------------------------------------------------
-- Util

authHeader :: ToHttpApiData a => a -> Request -> Request
authHeader = bearer "Authorization"

zOAuthHeader :: ToHttpApiData a => a -> Request -> Request
zOAuthHeader = bearer "Z-OAuth"

bearer :: ToHttpApiData a => HeaderName -> a -> Request -> Request
bearer name = header name . toHeader . Bearer

newOAuthClientRequestBody :: Text -> Text -> NewOAuthClient
newOAuthClientRequestBody name url =
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' (cs url)
      applicationName = OAuthApplicationName (unsafeRange name)
   in NewOAuthClient applicationName redirectUrl

registerNewOAuthClient :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> NewOAuthClient -> m OAuthClientCredentials
registerNewOAuthClient brig reqBody =
  responseJsonError =<< registerNewOAuthClient' brig reqBody <!! const 200 === statusCode

registerNewOAuthClient' :: (MonadIO m, MonadHttp m, HasCallStack) => Brig -> NewOAuthClient -> m ResponseLBS
registerNewOAuthClient' brig reqBody =
  post (brig . paths ["i", "oauth", "clients"] . json reqBody)

getOAuthClientInfo :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> OAuthClientId -> m OAuthClient
getOAuthClientInfo brig uid cid =
  responseJsonError =<< getOAuthClientInfo' brig uid cid <!! const 200 === statusCode

getOAuthClientInfo' :: (MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> OAuthClientId -> m ResponseLBS
getOAuthClientInfo' brig uid cid =
  get (brig . paths ["oauth", "clients", toByteString' cid] . zUser uid)

createOAuthCode :: (MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> NewOAuthAuthCode -> m ResponseLBS
createOAuthCode brig uid reqBody = post (brig . paths ["oauth", "authorization", "codes"] . zUser uid . json reqBody . noRedirect)

createOAuthAccessToken :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> OAuthAccessTokenRequest -> m OAuthAccessTokenResponse
createOAuthAccessToken brig reqBody = responseJsonError =<< createOAuthAccessToken' brig reqBody <!! const 200 === statusCode

createOAuthAccessToken' :: (MonadIO m, MonadHttp m, HasCallStack) => Brig -> OAuthAccessTokenRequest -> m ResponseLBS
createOAuthAccessToken' brig reqBody = do
  post (brig . paths ["oauth", "token"] . content "application/x-www-form-urlencoded" . body (RequestBodyLBS $ urlEncodeAsForm reqBody))

generateOAuthClientAndAuthCode :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> OAuthScopes -> RedirectUrl -> m (OAuthClientId, OAuthClientPlainTextSecret, OAuthAuthCode)
generateOAuthClientAndAuthCode brig uid scope url = do
  let newOAuthClient = NewOAuthClient (OAuthApplicationName (unsafeRange "E Corp")) url
  OAuthClientCredentials cid secret <- registerNewOAuthClient brig newOAuthClient
  (cid,secret,) <$> generateOAuthAuthCode brig uid cid scope url

generateOAuthAuthCode :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> OAuthClientId -> OAuthScopes -> RedirectUrl -> m OAuthAuthCode
generateOAuthAuthCode brig uid cid scope url = do
  let state = "foobar"
  response <-
    createOAuthCode brig uid (NewOAuthAuthCode cid scope OAuthResponseTypeCode url state) <!! do
      const 302 === statusCode
  pure $ fromMaybe (error "oauth auth code generation failed") $ (getHeader "Location" >=> fromByteString >=> getQueryParamValue "code" >=> fromByteString) response
  where
    getQueryParams :: RedirectUrl -> [(ByteString, ByteString)]
    getQueryParams (RedirectUrl uri) = uri ^. (queryL . queryPairsL)

    getQueryParamValue :: ByteString -> RedirectUrl -> Maybe ByteString
    getQueryParamValue key uri = snd <$> find ((== key) . fst) (getQueryParams uri)

signJwtToken :: JWK -> OAuthsClaimSet -> Http SignedJWT
signJwtToken key claims = do
  jwtOrError <- liftIO $ doSignClaims
  either (const $ error "jwt error") pure jwtOrError
  where
    doSignClaims :: IO (Either JWTError SignedJWT)
    doSignClaims = runJOSE $ do
      algo <- bestJWSAlg key
      signJWT key (newJWSHeader ((), algo)) claims

badKey :: JWK
badKey = do
  fromMaybe (error "invalid jwk") . A.decode $
    [r| {"kty":"OKP","crv":"Ed25519","x":"VWWycQ0yCoKAwN-DjjyQVWMRan1VOFUGlZpSTaLw1OA","d":"kH9sO4hDmRQi31IncBvwiaRB9xo9UHVaSvfV7RWiQOg"} |]

instance ToHttpApiData a => ToHttpApiData (Bearer a) where
  toHeader = (<>) "Bearer " . toHeader . unBearer
  toUrlPiece = T.decodeUtf8 . toHeader
