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

module API.OAuth (tests) where

import API.Team.Util qualified as Team
import Bilge
import Bilge.Assert
import Brig.API.OAuth hiding (verifyRefreshToken)
import Brig.Options
import Brig.Options qualified as Opt
import Cassandra qualified as C
import Control.Lens
import Control.Monad.Catch (MonadCatch)
import Crypto.JOSE (JOSE, JWK, bestJWSAlg, newJWSHeader, runJOSE)
import Crypto.JWT (Audience (Audience), ClaimsSet, JWTError, NumericDate (NumericDate), SignedJWT, claimAud, claimExp, claimIat, claimIss, claimSub, defaultJWTValidationSettings, emptyClaimsSet, signClaims, signJWT, stringOrUri, verifyClaims)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion (fromByteString, toByteString')
import Data.Domain (domainText)
import Data.Id
import Data.Qualified (Qualified (qUnqualified))
import Data.Range
import Data.Set as Set hiding (delete, null, (\\))
import Data.String.Conversions
import Data.Text.Ascii (encodeBase16)
import Data.Text.Encoding qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Imports
import Network.HTTP.Types (HeaderName)
import Network.Wai.Utilities qualified as Error
import Servant.API
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ
import URI.ByteString
import Util
import Web.FormUrlEncoded
import Wire.API.Conversation
import Wire.API.Conversation qualified as Conv
import Wire.API.Conversation.Code (CreateConversationCodeRequest (CreateConversationCodeRequest))
import Wire.API.Conversation.Role qualified as Role
import Wire.API.OAuth
import Wire.API.Routes.Bearer (Bearer (Bearer, unBearer))
import Wire.API.User as User
import Wire.API.User.Auth (CookieType (PersistentCookie))
import Wire.Sem.Jwk (readJwk)

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
          test m "wrong code fail" $ testCreateAccessTokenWrongAuthorizationCode b,
          test m "wrong redirect url fail" $ testCreateAccessTokenWrongUrl b,
          test m "expired code fail" $ testCreateAccessTokenExpiredCode o b,
          test m "wrong grant type fail" $ testCreateAccessTokenWrongGrantType b,
          test m "wrong code challenge fail" $ testCreateAccessTokenWrongCodeChallenge b,
          test m "wrong code verifier fail" $ testCreateAccessTokenWrongCodeVerifier b
        ],
      testGroup
        "access denied when disabled"
        [ test m "register" $ testRegisterOAuthClientAccessDeniedWhenDisabled o b,
          test m "get client info" $ testGetOAuthClientInfoAccessDeniedWhenDisabled o b,
          test m "create code" $ testCreateCodeOAuthClientAccessDeniedWhenDisabled o b,
          test m "create token" $ testCreateAccessTokenAccessDeniedWhenDisabled o b,
          test m "refresh access token" $ testRefreshAccessTokenAccessDeniedWhenDisabled o b
        ],
      testGroup
        "accessing a resource"
        [ test m "success" $ testAccessResourceSuccessNginz b n,
          test m "failure - insufficient scope" $ testAccessResourceInsufficientScope b n,
          test m "failure - expired token" $ testAccessResourceExpiredToken b n,
          test m "failure - nonsense token" $ testAccessResourceNonsenseToken n,
          test m "failure - no token" $ testAccessResourceNoToken n,
          test m "failure - invalid signature" $ testAccessResourceInvalidSignature o b n
        ],
      testGroup
        "accessing resources (only testing happy path to ensure scopes are valid)"
        [ test m "write:conversations" $ testWriteConversationsSuccessNginz b n,
          test m "read:feature_configs" $ testReadFeatureConfigsSuccessNginz b n,
          test m "write:conversations_code" $ testWriteConversationsCodeSuccessNginz b n
        ],
      testGroup
        "refresh tokens"
        [ test m "max active tokens" $ testRefreshTokenMaxActiveTokens o db b,
          test m "refresh access token - success" $ testRefreshTokenRetrieveAccessToken b n,
          test m "wrong signature - fail" $ testRefreshTokenWrongSignature o b,
          test m "no token id - fail" $ testRefreshTokenNoTokenId o b,
          test m "non-existing id - fail" $ testRefreshTokenNonExistingId o b,
          test m "wrong client id - fail" $ testRefreshTokenWrongClientId b,
          test m "wrong grant type - fail" $ testRefreshTokenWrongGrantType b,
          test m "expired token - fail" $ testRefreshTokenExpiredToken o b,
          test m "revoked token - fail" $ testRefreshTokenRevokedToken b
        ],
      testGroup
        "oauth applications"
        [ test m "list applications with account access" $ testListApplicationsWithAccountAccess b
        ]
    ]

testRegisterNewOAuthClient :: Brig -> Http ()
testRegisterNewOAuthClient brig = do
  let newOAuthClient@(OAuthClientConfig expectedAppName expectedUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  c <- registerNewOAuthClient brig newOAuthClient
  uid <- randomId
  oauthClientInfo <- getOAuthClientInfo brig uid c.clientId
  liftIO $ do
    expectedAppName @?= oauthClientInfo.name
    expectedUrl @?= oauthClientInfo.redirectUrl

testCreateOAuthCodeSuccess :: Brig -> Http ()
testCreateOAuthCodeSuccess brig = do
  let newOAuthClient@(OAuthClientConfig _ redirectUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  c <- registerNewOAuthClient brig newOAuthClient
  uid <- randomId
  let scope = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  state <- UUID.toText <$> liftIO nextRandom

  createOAuthCode brig uid (CreateOAuthAuthorizationCodeRequest c.clientId scope OAuthResponseTypeCode redirectUrl state S256 challenge) !!! do
    const 201 === statusCode
    const (Just $ unRedirectUrl redirectUrl ^. pathL) === (fmap getPath . getLocation)
    const (Just $ ["code", "state"]) === (fmap (fmap fst . getQueryParams) . getLocation)
    const (Just $ cs state) === (getLocation >=> getQueryParamValue "state")
    const (Just True) === (getLocation >=> getQueryParamValue "code" >=> checkCode)
  where
    checkCode :: ByteString -> Maybe Bool
    checkCode bs = Just $ BS.all isHexDigit bs && BS.length bs == 64

testCreateOAuthCodeRedirectUrlMismatch :: Brig -> Http ()
testCreateOAuthCodeRedirectUrlMismatch brig = do
  let newOAuthClient = newOAuthClientRequestBody "E Corp" "https://example.com"
  c <- registerNewOAuthClient brig newOAuthClient
  uid <- randomId
  state <- UUID.toText <$> liftIO nextRandom
  let differentUrl = mkUrl "https://wire.com"
  createOAuthCode brig uid (CreateOAuthAuthorizationCodeRequest c.clientId mempty OAuthResponseTypeCode differentUrl state S256 challenge) !!! do
    const 400 === statusCode
    const Nothing === (fmap getPath . getLocation)
    const (Just "redirect-url-miss-match") === fmap Error.label . responseJsonMaybe

testCreateOAuthCodeClientNotFound :: Brig -> Http ()
testCreateOAuthCodeClientNotFound brig = do
  cid <- randomId
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  state <- UUID.toText <$> liftIO nextRandom
  createOAuthCode brig uid (CreateOAuthAuthorizationCodeRequest cid mempty OAuthResponseTypeCode redirectUrl state S256 challenge) !!! do
    const 404 === statusCode
    const (Just $ "access_denied") === (getLocation >=> getQueryParamValue "error")
    const (Just $ cs state) === (getLocation >=> getQueryParamValue "state")
    const (Just $ unRedirectUrl redirectUrl ^. pathL) === (fmap getPath . getLocation)
    const Nothing === (getLocation >=> getQueryParamValue "code")

testCreateAccessTokenSuccess :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenSuccess opts brig = do
  now <- liftIO getCurrentTime
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.singleton ReadSelf
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  -- authorization code should be deleted and can only be used once
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe
  k <- liftIO $ readJwk (fromMaybe "path to jwk not set" opts.settings.oAuthJwkKeyPair) <&> fromMaybe (error "invalid key")
  verifiedOrError <- liftIO $ verify k (unOAuthToken $ resp.accessToken)
  verifiedOrErrorWithotherKey <- liftIO $ verify badKey (unOAuthToken $ resp.accessToken)
  let expectedDomain = domainText opts.settings.federationDomain
  liftIO $ do
    isRight verifiedOrError @?= True
    isLeft verifiedOrErrorWithotherKey @?= True
    let claims = either (error "invalid token") id verifiedOrError
    claims.scope @?= scopes
    (view claimIss $ claims) @?= (expectedDomain ^? stringOrUri @Text)
    (view claimAud $ claims) @?= (Audience . (: []) <$> expectedDomain ^? stringOrUri @Text)
    (view claimSub $ claims) @?= (idToText (User.userId user) ^? stringOrUri)
    let expTime = (\(NumericDate x) -> x) . fromMaybe (error "exp claim missing") . view claimExp $ claims
    diffUTCTime expTime now > 0 @?= True
    let issuingTime = (\(NumericDate x) -> x) . fromMaybe (error "iat claim missing") . view claimIat $ claims
    abs (diffUTCTime issuingTime now) < 5 @?= True -- allow for some generous clock skew

testCreateAccessTokenWrongClientId :: Brig -> Http ()
testCreateAccessTokenWrongClientId brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  (_, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
  cid <- randomId
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongAuthorizationCode :: Brig -> Http ()
testCreateAccessTokenWrongAuthorizationCode brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  (cid, _) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
  let code = OAuthAuthorizationCode $ encodeBase16 "eb32eb9e2aa36c081c89067dddf81bce83c1c57e0b74cfb14c9f026f145f2b1f"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongUrl :: Brig -> Http ()
testCreateAccessTokenWrongUrl brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://wire.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
  let wrongUrl = mkUrl "https://example.com"
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code wrongUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 400 === statusCode
    const (Just "redirect-url-miss-match") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenExpiredCode :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenExpiredCode opts brig =
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthAuthorizationCodeExpirationTimeSecsInternalLens ?~ 1) $ do
    uid <- randomId
    let redirectUrl = mkUrl "https://example.com"
    let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
    (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
    liftIO $ threadDelay (1 * 1200 * 1000)
    let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
    createOAuthAccessToken' brig accessTokenRequest !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe

testCreateAccessTokenWrongGrantType :: Brig -> Http ()
testCreateAccessTokenWrongGrantType brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeRefreshToken cid verifier code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! assertAccessDenied

testCreateAccessTokenWrongCodeChallenge :: Brig -> Http ()
testCreateAccessTokenWrongCodeChallenge brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  (cid, code) <- generateOAuthClientAndAuthorizationCode' wrongCodeChallenge brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 403 === statusCode
    const (Just "invalid_grant") === fmap Error.label . responseJsonMaybe
  where
    wrongCodeChallenge :: OAuthCodeChallenge
    wrongCodeChallenge = fromMaybe (error $ "invalid code challenge") $ A.decode "\"kw8DtStRIz2MTWyG59pd9h2Kyfhoa8SM4aU8CUWM1DU\""

testCreateAccessTokenWrongCodeVerifier :: Brig -> Http ()
testCreateAccessTokenWrongCodeVerifier brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid wrongCodeVerifier code redirectUrl
  createOAuthAccessToken' brig accessTokenRequest !!! do
    const 403 === statusCode
    const (Just "invalid_grant") === fmap Error.label . responseJsonMaybe
  where
    wrongCodeVerifier :: OAuthCodeVerifier
    wrongCodeVerifier = fromMaybe (error "invalid code verifier") $ A.decode "\"x9xpNj_TNfXY5h-CggZozno7ldzPmbEh8al~HJQmfiZtvvx0uxlDa~mNCZrH37XZnClD71Vx_Edx8FU1XU2mt38.o49Wnca~at75RBoxHn..F-_n5kveOSCpc_Oemyap\""

testGetOAuthClientInfoAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testGetOAuthClientInfoAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthEnabledInternalLens ?~ False) $ do
    cid <- randomId
    uid <- randomId
    getOAuthClientInfo' brig uid cid !!! assertAccessDenied

testCreateCodeOAuthClientAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testCreateCodeOAuthClientAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthEnabledInternalLens ?~ False) $ do
    cid <- randomId
    uid <- randomId
    state <- UUID.toText <$> liftIO nextRandom
    let redirectUrl = mkUrl "https://example.com"
    createOAuthCode brig uid (CreateOAuthAuthorizationCodeRequest cid mempty OAuthResponseTypeCode redirectUrl state S256 challenge) !!! do
      const 403 === statusCode
      const (Just $ "access_denied") === (getLocation >=> getQueryParamValue "error")
      const (Just $ cs state) === (getLocation >=> getQueryParamValue "state")
      const (Just $ unRedirectUrl redirectUrl ^. pathL) === (fmap getPath . getLocation)
      const Nothing === (getLocation >=> getQueryParamValue "code")

testCreateAccessTokenAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testCreateAccessTokenAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthEnabledInternalLens ?~ False) $ do
    cid <- randomId
    let code = OAuthAuthorizationCode $ encodeBase16 "eb32eb9e2aa36c081c89067dddf81bce83c1c57e0b74cfb14c9f026f145f2b1f"
    let url = mkUrl "https://example.com"
    let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code url
    createOAuthAccessToken' brig accessTokenRequest !!! assertAccessDenied

testRefreshAccessTokenAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testRefreshAccessTokenAccessDeniedWhenDisabled opts brig = do
  uid <- randomId
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthEnabledInternalLens ?~ False) $ do
    let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid resp.refreshToken
    refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! assertAccessDenied

testRegisterOAuthClientAccessDeniedWhenDisabled :: Opt.Opts -> Brig -> Http ()
testRegisterOAuthClientAccessDeniedWhenDisabled opts brig =
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthEnabledInternalLens ?~ False) $ do
    let newOAuthClient = newOAuthClientRequestBody "E Corp" "https://example.com"
    registerNewOAuthClient' brig newOAuthClient !!! assertAccessDenied

assertAccessDenied :: Assertions ()
assertAccessDenied = do
  const 403 === statusCode
  const (Just "forbidden") === fmap Error.label . responseJsonMaybe

testAccessResourceSuccessNginz :: Brig -> Nginz -> Http ()
testAccessResourceSuccessNginz brig nginz = do
  -- with ZAuth header
  user <- createUser "alice" brig
  let email = fromMaybe (error "no email") $ userEmail user
  zauthToken <- decodeToken <$> (login nginz (defEmailLogin email) PersistentCookie <!! const 200 === statusCode)
  self :: SelfProfile <-
    responseJsonError
      =<< get (nginz . path "/self" . header "Authorization" ("Bearer " <> toByteString' zauthToken)) <!! const 200 === statusCode

  -- with Authorization header containing an OAuth bearer token
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  self' <- responseJsonError =<< get (nginz . paths ["self"] . authHeader resp.accessToken) <!! const 200 === statusCode
  liftIO $ self @?= self'

testAccessResourceInsufficientScope :: Brig -> Nginz -> Http ()
testAccessResourceInsufficientScope brig nginz = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [WriteConversations]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  get (nginz . paths ["self"] . authHeader resp.accessToken) !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testAccessResourceExpiredToken :: Brig -> Nginz -> Http ()
testAccessResourceExpiredToken brig nginz = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  liftIO $ threadDelay (5 * 1000 * 1000)
  get (nginz . paths ["self"] . authHeader resp.accessToken) !!! do
    const 401 === statusCode
    const "Unauthorized" === statusMessage

testAccessResourceNonsenseToken :: Nginz -> Http ()
testAccessResourceNonsenseToken nginz = do
  get (nginz . paths ["self"] . authHeader @Text "foo") !!! do
    const 401 === statusCode
    const "Unauthorized" === statusMessage

testAccessResourceNoToken :: Nginz -> Http ()
testAccessResourceNoToken nginz =
  get (nginz . paths ["self"]) !!! do
    const 401 === statusCode
    const "Unauthorized" === statusMessage

testAccessResourceInvalidSignature :: Opt.Opts -> Brig -> Nginz -> Http ()
testAccessResourceInvalidSignature opts brig nginz = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  key <- liftIO $ readJwk (fromMaybe "path to jwk not set" opts.settings.oAuthJwkKeyPair) <&> fromMaybe (error "invalid key")
  claimSet <- fromRight (error "token invalid") <$> liftIO (verify key (unOAuthToken $ resp.accessToken))
  tokenSignedWithotherKey <- signAccessToken badKey claimSet
  get (nginz . paths ["self"] . authHeader (OAuthToken tokenSignedWithotherKey)) !!! do
    const 401 === statusCode
    const "Unauthorized" === statusMessage

testRefreshTokenMaxActiveTokens :: Opts -> C.ClientState -> Brig -> Http ()
testRefreshTokenMaxActiveTokens opts db brig =
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthMaxActiveRefreshTokensInternalLens ?~ 2) $ do
    uid <- randomId
    jwk <- liftIO $ readJwk (fromMaybe "path to jwk not set" opts.settings.oAuthJwkKeyPair) <&> fromMaybe (error "invalid key")
    let redirectUrl = mkUrl "https://example.com"
    let scopes = OAuthScopes $ Set.fromList [WriteConversations, WriteConversationsCode]
    let delayOneSec =
          -- we have to wait ~1 sec before we create the next token, to make sure it is created with a different timestamp
          -- this is due to the interpreter of the `Now` effect which auto-updates every second
          -- FUTUREWORK: once the interpreter of the `Now` effect is changed to use a monotonic clock, we can remove this delay
          threadDelay $ 1000 * 1000
    (rid1, cid, _) <- do
      let testMsg = "0 active refresh tokens - 1st requested token will be active"
      (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
      resp <- createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk resp.refreshToken
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid] `hasSameElems` ((.refreshTokenId) <$> tokens)
      pure (rid, cid, secret)
    delayOneSec
    rid2 <- do
      let testMsg = "1 active refresh token - 2nd requested token will added to active tokens"
      code <- generateOAuthAuthorizationCode brig uid cid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
      resp <- createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk resp.refreshToken
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid1, rid] `hasSameElems` ((.refreshTokenId) <$> tokens)
      pure rid
    delayOneSec
    rid3 <- do
      let testMsg = "2 active refresh tokens - 3rd token requested replaces the 1st one"
      code <- generateOAuthAuthorizationCode brig uid cid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
      resp <- createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk resp.refreshToken
      recoverN 3 $ do
        tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
        liftIO $ assertBool testMsg $ [rid2, rid] `hasSameElems` ((.refreshTokenId) <$> tokens)
      pure rid
    delayOneSec
    do
      let testMsg = "2 active refresh tokens - 4th token requests replaces the 2nd one"
      code <- generateOAuthAuthorizationCode brig uid cid scopes redirectUrl
      let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
      resp <- createOAuthAccessToken brig accessTokenRequest
      rid <- extractRefreshTokenId jwk resp.refreshToken
      tokens <- C.runClient db (lookupOAuthRefreshTokens uid)
      liftIO $ assertBool testMsg $ [rid3, rid] `hasSameElems` ((.refreshTokenId) <$> tokens)
  where
    extractRefreshTokenId :: (MonadIO m) => JWK -> OAuthRefreshToken -> m OAuthRefreshTokenId
    extractRefreshTokenId jwk rt = do
      fromMaybe (error "invalid sub") . hcsSub <$> liftIO (verifyRefreshToken jwk (unOAuthToken rt))

    hasSameElems :: (Ord a) => [a] -> [a] -> Bool
    hasSameElems (Set.fromList -> x) (Set.fromList -> y) = x == y

testRefreshTokenRetrieveAccessToken :: Brig -> Nginz -> Http ()
testRefreshTokenRetrieveAccessToken brig nginz = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  get (nginz . paths ["self"] . authHeader (resp.accessToken)) !!! const 200 === statusCode
  threadDelay $ 5 * 1000 * 1000 -- wait 5 seconds for access token to expire
  get (nginz . paths ["self"] . authHeader (resp.accessToken)) !!! const 401 === statusCode
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid resp.refreshToken
  resp' <- refreshOAuthAccessToken brig refreshAccessTokenRequest
  get (nginz . paths ["self"] . authHeader resp'.accessToken) !!! const 200 === statusCode

testRefreshTokenWrongSignature :: Opts -> Brig -> Http ()
testRefreshTokenWrongSignature opts brig = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  key <- liftIO $ readJwk (fromMaybe "path to jwk not set" opts.settings.oAuthJwkKeyPair) <&> fromMaybe (error "invalid key")
  badRefreshToken <- liftIO $ do
    claims <- verifyRefreshToken key (unOAuthToken $ resp.refreshToken)
    OAuthToken <$> signRefreshToken badKey claims
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid badRefreshToken
  refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testRefreshTokenNoTokenId :: Opts -> Brig -> Http ()
testRefreshTokenNoTokenId opts brig = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, _) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  key <- liftIO $ readJwk (fromMaybe "path to jwk not set" opts.settings.oAuthJwkKeyPair) <&> fromMaybe (error "invalid key")
  badRefreshToken <- liftIO $ OAuthToken <$> signRefreshToken key emptyClaimsSet
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid badRefreshToken
  refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testRefreshTokenNonExistingId :: Opts -> Brig -> Http ()
testRefreshTokenNonExistingId opts brig = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  key <- liftIO $ readJwk (fromMaybe "path to jwk not set" opts.settings.oAuthJwkKeyPair) <&> fromMaybe (error "invalid key")
  badRefreshToken <-
    liftIO $
      OAuthToken <$> do
        claims <- verifyRefreshToken key (unOAuthToken $ resp.refreshToken)
        rid :: OAuthRefreshTokenId <- randomId
        sub <- maybe (error "creating sub claim failed") pure $ idToText rid ^? stringOrUri
        let invalidClaims = claims & claimSub ?~ sub
        signRefreshToken key invalidClaims
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid badRefreshToken
  refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testRefreshTokenWrongClientId :: Brig -> Http ()
testRefreshTokenWrongClientId brig = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  badCid <- randomId
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken badCid resp.refreshToken
  refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testRefreshTokenWrongGrantType :: Brig -> Http ()
testRefreshTokenWrongGrantType brig = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeAuthorizationCode cid resp.refreshToken
  refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testRefreshTokenExpiredToken :: Opts -> Brig -> Http ()
testRefreshTokenExpiredToken opts brig =
  -- overriding settings and set refresh token to expire in 2 seconds
  withSettingsOverrides (opts & Opt.settingsLens . Opt.oAuthRefreshTokenExpirationTimeSecsInternalLens ?~ 2) $ do
    user <- createUser "alice" brig
    let redirectUrl = mkUrl "https://example.com"
    let scopes = OAuthScopes $ Set.fromList [ReadSelf]
    (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
    let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
    resp <- createOAuthAccessToken brig accessTokenRequest
    let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid resp.refreshToken
    threadDelay $ 2 * 1010 * 1000 -- wait for 2 seconds for the token to expire
    refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
      const 403 === statusCode
      const "Forbidden" === statusMessage

testRefreshTokenRevokedToken :: Brig -> Http ()
testRefreshTokenRevokedToken brig = do
  user <- createUser "alice" brig
  let redirectUrl = mkUrl "https://example.com"
  let scopes = OAuthScopes $ Set.fromList [ReadSelf]
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig (User.userId user) scopes redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  resp <- createOAuthAccessToken brig accessTokenRequest
  let refreshAccessTokenRequest = OAuthRefreshAccessTokenRequest OAuthGrantTypeRefreshToken cid resp.refreshToken
  revokeOAuthRefreshToken brig (OAuthRevokeRefreshTokenRequest cid resp.refreshToken) !!! const 200 === statusCode
  refreshOAuthAccessToken' brig refreshAccessTokenRequest !!! do
    const 403 === statusCode
    const "Forbidden" === statusMessage

testListApplicationsWithAccountAccess :: Brig -> Http ()
testListApplicationsWithAccountAccess brig = do
  alice <- createUser "alice" brig
  bob <- createUser "bob" brig
  do
    apps <- listOAuthApplications brig (User.userId alice)
    liftIO $ apps @?= []
  void $ createOAuthApplicationWithAccountAccess brig (User.userId alice)
  void $ createOAuthApplicationWithAccountAccess brig (User.userId alice)
  do
    aliceApps <- listOAuthApplications brig (User.userId alice)
    liftIO $ assertEqual "apps" 2 (length aliceApps)
    bobsApps <- listOAuthApplications brig (User.userId bob)
    liftIO $ bobsApps @?= []
  void $ createOAuthApplicationWithAccountAccess brig (User.userId alice)
  void $ createOAuthApplicationWithAccountAccess brig (User.userId bob)
  do
    aliceApps <- listOAuthApplications brig (User.userId alice)
    liftIO $ assertEqual "apps" 3 (length aliceApps)
    bobsApps <- listOAuthApplications brig (User.userId bob)
    liftIO $ assertEqual "apps" 1 (length bobsApps)

testWriteConversationsSuccessNginz :: Brig -> Nginz -> Http ()
testWriteConversationsSuccessNginz brig nginz = do
  (uid, tid) <- Team.createUserWithTeam brig
  resp <- getAccessTokenForScope brig uid [WriteConversations]
  createTeamConv nginz authHeader resp.accessToken tid "oauth test group" !!! do
    const 201 === statusCode

testReadFeatureConfigsSuccessNginz :: Brig -> Nginz -> Http ()
testReadFeatureConfigsSuccessNginz brig nginz = do
  (uid, _) <- Team.createUserWithTeam brig
  resp <- getAccessTokenForScope brig uid [ReadFeatureConfigs]
  getFeatureConfigs nginz authHeader resp.accessToken !!! do
    const 200 === statusCode

testWriteConversationsCodeSuccessNginz :: Brig -> Nginz -> Http ()
testWriteConversationsCodeSuccessNginz brig nginz = do
  (uid, tid) <- Team.createUserWithTeam brig
  resp <- getAccessTokenForScope brig uid [WriteConversations, WriteConversationsCode]
  conv <-
    responseJsonError
      =<< createTeamConv nginz authHeader resp.accessToken tid "oauth test group" <!! do
        const 201 === statusCode
  postConvCode nginz authHeader resp.accessToken (qUnqualified . cnvQualifiedId $ conv) !!! do
    const 201 === statusCode

-------------------------------------------------------------------------------
-- Util

postConvCode :: (Request -> Request) -> (OAuthAccessToken -> Request -> Request) -> OAuthAccessToken -> ConvId -> Http ResponseLBS
postConvCode svc mkHeader token c = do
  post $
    svc
      . paths ["conversations", toByteString' c, "code"]
      . mkHeader token
      . json (CreateConversationCodeRequest Nothing)

getAccessTokenForScope :: Brig -> UserId -> [OAuthScope] -> Http OAuthAccessTokenResponse
getAccessTokenForScope brig uid scopes = do
  let redirectUrl = mkUrl "https://example.com"
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid (OAuthScopes $ Set.fromList scopes) redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  createOAuthAccessToken brig accessTokenRequest

createTeamConv ::
  (Request -> Request) ->
  (OAuthAccessToken -> Request -> Request) ->
  OAuthAccessToken ->
  TeamId ->
  Text ->
  Http ResponseLBS
createTeamConv svc mkHeader token tid name = do
  let tinfo = Conv.ConvTeamInfo tid
  let conv = Conv.NewConv [] [] (checked name) (Set.fromList [CodeAccess]) Nothing (Just tinfo) Nothing Nothing Role.roleNameWireAdmin BaseProtocolProteusTag GroupConversation
  post $
    svc
      . path "conversations"
      . mkHeader token
      . json conv

getFeatureConfigs ::
  (Request -> Request) ->
  (OAuthAccessToken -> Request -> Request) ->
  OAuthAccessToken ->
  Http ResponseLBS
getFeatureConfigs svc mkHeader token = do
  get $
    svc
      . path "feature-configs"
      . mkHeader token

createOAuthApplicationWithAccountAccess :: Brig -> UserId -> Http OAuthAccessTokenResponse
createOAuthApplicationWithAccountAccess brig uid = do
  let redirectUrl = mkUrl "https://example.com"
  (cid, code) <- generateOAuthClientAndAuthorizationCode brig uid (OAuthScopes $ mempty) redirectUrl
  let accessTokenRequest = OAuthAccessTokenRequest OAuthGrantTypeAuthorizationCode cid verifier code redirectUrl
  createOAuthAccessToken brig accessTokenRequest

verifyRefreshToken :: JWK -> SignedJWT -> IO ClaimsSet
verifyRefreshToken jwk jwt =
  fromRight (error "invalid jwt or jwk")
    <$> runJOSE (verifyClaims (defaultJWTValidationSettings (const True)) jwk jwt :: JOSE JWTError IO ClaimsSet)

authHeader :: (ToHttpApiData a) => a -> Request -> Request
authHeader = bearer "Authorization"

bearer :: (ToHttpApiData a) => HeaderName -> a -> Request -> Request
bearer name = header name . toHeader . Bearer

newOAuthClientRequestBody :: Text -> Text -> OAuthClientConfig
newOAuthClientRequestBody name url =
  let redirectUrl = mkUrl (cs url)
      applicationName = OAuthApplicationName (unsafeRange name)
   in OAuthClientConfig applicationName redirectUrl

registerNewOAuthClient :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> OAuthClientConfig -> m OAuthClientCredentials
registerNewOAuthClient brig reqBody =
  responseJsonError =<< registerNewOAuthClient' brig reqBody <!! const 200 === statusCode

registerNewOAuthClient' :: (MonadHttp m) => Brig -> OAuthClientConfig -> m ResponseLBS
registerNewOAuthClient' brig reqBody =
  post (brig . paths ["i", "oauth", "clients"] . json reqBody)

getOAuthClientInfo :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> OAuthClientId -> m OAuthClient
getOAuthClientInfo brig uid cid =
  responseJsonError =<< getOAuthClientInfo' brig uid cid <!! const 200 === statusCode

getOAuthClientInfo' :: (MonadHttp m) => Brig -> UserId -> OAuthClientId -> m ResponseLBS
getOAuthClientInfo' brig uid cid =
  get (brig . paths ["oauth", "clients", toByteString' cid] . zUser uid)

createOAuthCode :: (MonadHttp m) => Brig -> UserId -> CreateOAuthAuthorizationCodeRequest -> m ResponseLBS
createOAuthCode brig uid reqBody = post (brig . paths ["oauth", "authorization", "codes"] . zUser uid . json reqBody . noRedirect)

createOAuthAccessToken :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> OAuthAccessTokenRequest -> m OAuthAccessTokenResponse
createOAuthAccessToken brig reqBody = responseJsonError =<< createOAuthAccessToken' brig reqBody <!! const 200 === statusCode

createOAuthAccessToken' :: (MonadHttp m) => Brig -> OAuthAccessTokenRequest -> m ResponseLBS
createOAuthAccessToken' brig reqBody = do
  post (brig . paths ["oauth", "token"] . content "application/x-www-form-urlencoded" . body (RequestBodyLBS $ urlEncodeAsForm reqBody))

refreshOAuthAccessToken :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> OAuthRefreshAccessTokenRequest -> m OAuthAccessTokenResponse
refreshOAuthAccessToken brig reqBody =
  responseJsonError =<< refreshOAuthAccessToken' brig reqBody <!! const 200 === statusCode

refreshOAuthAccessToken' :: (MonadHttp m) => Brig -> OAuthRefreshAccessTokenRequest -> m ResponseLBS
refreshOAuthAccessToken' brig reqBody =
  post (brig . paths ["oauth", "token"] . content "application/x-www-form-urlencoded" . body (RequestBodyLBS $ urlEncodeAsForm reqBody))

listOAuthApplications' :: (MonadHttp m) => Brig -> UserId -> m ResponseLBS
listOAuthApplications' brig uid =
  get (brig . paths ["oauth", "applications"] . zUser uid)

listOAuthApplications :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> m [OAuthApplication]
listOAuthApplications brig uid =
  responseJsonError =<< listOAuthApplications' brig uid <!! const 200 === statusCode

generateOAuthClientAndAuthorizationCode :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> OAuthScopes -> RedirectUrl -> m (OAuthClientId, OAuthAuthorizationCode)
generateOAuthClientAndAuthorizationCode = generateOAuthClientAndAuthorizationCode' challenge

generateOAuthClientAndAuthorizationCode' :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => OAuthCodeChallenge -> Brig -> UserId -> OAuthScopes -> RedirectUrl -> m (OAuthClientId, OAuthAuthorizationCode)
generateOAuthClientAndAuthorizationCode' chal brig uid scope url = do
  let newOAuthClient = OAuthClientConfig (OAuthApplicationName (unsafeRange "E Corp")) url
  OAuthClientCredentials cid _ <- registerNewOAuthClient brig newOAuthClient
  (cid,) <$> generateOAuthAuthorizationCode' chal brig uid cid scope url

generateOAuthAuthorizationCode :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> UserId -> OAuthClientId -> OAuthScopes -> RedirectUrl -> m OAuthAuthorizationCode
generateOAuthAuthorizationCode = generateOAuthAuthorizationCode' challenge

generateOAuthAuthorizationCode' :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => OAuthCodeChallenge -> Brig -> UserId -> OAuthClientId -> OAuthScopes -> RedirectUrl -> m OAuthAuthorizationCode
generateOAuthAuthorizationCode' chal brig uid cid scope url = do
  state <- UUID.toText <$> liftIO nextRandom
  response <-
    createOAuthCode brig uid (CreateOAuthAuthorizationCodeRequest cid scope OAuthResponseTypeCode url state S256 chal) <!! do
      const 201 === statusCode
  pure $ fromMaybe (error "oauth auth code generation failed") $ (getHeader "Location" >=> fromByteString >=> getQueryParamValue "code" >=> fromByteString) response

signAccessToken :: JWK -> OAuthClaimsSet -> Http SignedJWT
signAccessToken key claims = do
  jwtOrError <- liftIO $ doSignClaims
  either (const $ error "jwt error") pure jwtOrError
  where
    doSignClaims :: IO (Either JWTError SignedJWT)
    doSignClaims = runJOSE $ do
      algo <- bestJWSAlg key
      signJWT key (newJWSHeader ((), algo)) claims

signRefreshToken :: JWK -> ClaimsSet -> IO SignedJWT
signRefreshToken key claims = do
  jwtOrError <- doSignClaims
  either (const $ error "jwt error") pure jwtOrError
  where
    doSignClaims :: IO (Either JWTError SignedJWT)
    doSignClaims = runJOSE $ do
      algo <- bestJWSAlg key
      signClaims key (newJWSHeader ((), algo)) claims

badKey :: JWK
badKey = do
  fromMaybe (error "invalid jwk") . A.decode $
    [r| {"kty":"OKP","crv":"Ed25519","x":"VWWycQ0yCoKAwN-DjjyQVWMRan1VOFUGlZpSTaLw1OA","d":"kH9sO4hDmRQi31IncBvwiaRB9xo9UHVaSvfV7RWiQOg"} |]

mkUrl :: ByteString -> RedirectUrl
mkUrl = fromMaybe (error "invalid url") . fromByteString

revokeOAuthRefreshToken :: (MonadHttp m) => Brig -> OAuthRevokeRefreshTokenRequest -> m ResponseLBS
revokeOAuthRefreshToken brig req = post (brig . paths ["oauth", "revoke"] . json req)

instance (ToHttpApiData a) => ToHttpApiData (Bearer a) where
  toHeader = (<>) "Bearer " . toHeader . unBearer
  toUrlPiece = T.decodeUtf8 . toHeader

getLocation :: ResponseLBS -> Maybe RedirectUrl
getLocation = getHeader "Location" >=> fromByteString

getPath :: RedirectUrl -> ByteString
getPath (RedirectUrl uri) = uri ^. pathL

getQueryParams :: RedirectUrl -> [(ByteString, ByteString)]
getQueryParams (RedirectUrl uri) = uri ^. (queryL . queryPairsL)

getQueryParamValue :: ByteString -> RedirectUrl -> Maybe ByteString
getQueryParamValue key uri = snd <$> find ((== key) . fst) (getQueryParams uri)

challenge :: OAuthCodeChallenge
challenge = either (\e -> error $ "invalid code challenge " <> show e) id $ A.eitherDecode "\"G7CWLBqYDT8doT_oEIN3un_QwZWYKHmOqG91nwNzITc\""

verifier :: OAuthCodeVerifier
verifier = either (\e -> error $ "invalid code verifier " <> show e) id $ A.eitherDecode "\"nE3k3zykOmYki~kriKzAmeFiGT7cWugcuToFwo1YPgrZ1cFvaQqLa.dXY9MnDj3umAmG-8lSNIYIl31Cs_.fV5r2psa4WWZcB.Nlc3A-t3p67NDZaOJjIiH~8PvUH_hR\""
