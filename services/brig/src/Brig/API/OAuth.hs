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

module Brig.API.OAuth
  ( internalOauthAPI,
    oauthAPI,
    lookupOAuthRefreshTokens,
    verifyRefreshToken,
  )
where

import Brig.API.Error (throwStd)
import Brig.API.Handler (Handler)
import Brig.App
import Brig.Options qualified as Opt
import Cassandra hiding (Set)
import Cassandra qualified as C
import Control.Error (assertMay, failWith, failWithM)
import Control.Lens (view, (?~), (^?))
import Control.Monad.Except
import Crypto.JWT hiding (params, uri)
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Misc
import Data.Set qualified as Set
import Data.Text.Ascii
import Data.Time
import Imports hiding (exp)
import OpenSSL.Random (randBytes)
import Polysemy (Member)
import Servant hiding (Handler, Tagged)
import Wire.API.Error
import Wire.API.OAuth as OAuth
import Wire.API.Password (Password, mkSafePassword)
import Wire.API.Routes.Internal.Brig.OAuth qualified as I
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Brig.OAuth
import Wire.Sem.Jwk
import Wire.Sem.Jwk qualified as Jwk
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

--------------------------------------------------------------------------------
-- API Internal

internalOauthAPI :: ServerT I.OAuthAPI (Handler r)
internalOauthAPI =
  Named @"create-oauth-client" registerOAuthClient
    :<|> Named @"get-oauth-client" getOAuthClientById
    :<|> Named @"update-oauth-client" updateOAuthClient
    :<|> Named @"delete-oauth-client" deleteOAuthClient

--------------------------------------------------------------------------------
-- API Public

oauthAPI :: (Member Now r, Member Jwk r) => ServerT OAuthAPI (Handler r)
oauthAPI =
  Named @"get-oauth-client" getOAuthClient
    :<|> Named @"create-oauth-auth-code" createNewOAuthAuthorizationCode
    :<|> Named @"create-oauth-access-token" createAccessTokenWith
    :<|> Named @"revoke-oauth-refresh-token" revokeRefreshToken
    :<|> Named @"get-oauth-applications" getOAuthApplications
    :<|> Named @"revoke-oauth-account-access" revokeOAuthAccountAccess

--------------------------------------------------------------------------------
-- Handlers

registerOAuthClient :: OAuthClientConfig -> (Handler r) OAuthClientCredentials
registerOAuthClient (OAuthClientConfig name uri) = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  credentials@(OAuthClientCredentials cid secret) <- OAuthClientCredentials <$> randomId <*> createSecret
  safeSecret <- liftIO $ hashClientSecret secret
  lift $ wrapClient $ insertOAuthClient cid name uri safeSecret
  pure credentials
  where
    createSecret :: MonadIO m => m OAuthClientPlainTextSecret
    createSecret = OAuthClientPlainTextSecret <$> rand32Bytes

    hashClientSecret :: MonadIO m => OAuthClientPlainTextSecret -> m Password
    hashClientSecret = mkSafePassword . plainTextPassword8Unsafe . toText . unOAuthClientPlainTextSecret

rand32Bytes :: MonadIO m => m AsciiBase16
rand32Bytes = liftIO . fmap encodeBase16 $ randBytes 32

getOAuthClientById :: OAuthClientId -> (Handler r) OAuthClient
getOAuthClientById cid = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  mClient <- lift $ wrapClient $ lookupOauthClient cid
  maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure mClient

updateOAuthClient :: OAuthClientId -> OAuthClientConfig -> (Handler r) OAuthClient
updateOAuthClient cid config = do
  void $ getOAuthClientById cid
  lift $ wrapClient $ updateOAuthClient' cid config.applicationName config.redirectUrl
  getOAuthClientById cid

deleteOAuthClient :: OAuthClientId -> (Handler r) ()
deleteOAuthClient cid = do
  void $ getOAuthClientById cid
  lift $ wrapClient $ deleteOAuthClient' cid

--------------------------------------------------------------------------------

getOAuthClient :: UserId -> OAuthClientId -> (Handler r) (Maybe OAuthClient)
getOAuthClient _ cid = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  lift $ wrapClient $ lookupOauthClient cid

createNewOAuthAuthorizationCode :: UserId -> CreateOAuthAuthorizationCodeRequest -> (Handler r) CreateOAuthCodeResponse
createNewOAuthAuthorizationCode uid code = do
  runExceptT (validateAndCreateAuthorizationCode uid code) >>= \case
    Right oauthCode ->
      pure $ CreateOAuthCodeSuccess $ code.redirectUri & addParams [("code", toByteString' oauthCode), ("state", cs code.state)]
    Left CreateNewOAuthCodeErrorFeatureDisabled ->
      pure $ CreateOAuthCodeFeatureDisabled $ code.redirectUri & addParams [("error", "access_denied"), ("error_description", "OAuth is not enabled"), ("state", cs code.state)]
    Left CreateNewOAuthCodeErrorClientNotFound ->
      pure $ CreateOAuthCodeClientNotFound $ code.redirectUri & addParams [("error", "access_denied"), ("error_description", "The client ID was not found"), ("state", cs code.state)]
    Left CreateNewOAuthCodeErrorUnsupportedResponseType ->
      pure $ CreateOAuthCodeUnsupportedResponseType $ code.redirectUri & addParams [("error", "access_denied"), ("error_description", "The client ID was not found"), ("state", cs code.state)]
    Left CreateNewOAuthCodeErrorRedirectUrlMissMatch ->
      pure CreateOAuthCodeRedirectUrlMissMatch

data CreateNewOAuthCodeError
  = CreateNewOAuthCodeErrorFeatureDisabled
  | CreateNewOAuthCodeErrorClientNotFound
  | CreateNewOAuthCodeErrorUnsupportedResponseType
  | CreateNewOAuthCodeErrorRedirectUrlMissMatch

validateAndCreateAuthorizationCode :: UserId -> CreateOAuthAuthorizationCodeRequest -> ExceptT CreateNewOAuthCodeError (Handler r) OAuthAuthorizationCode
validateAndCreateAuthorizationCode uid (CreateOAuthAuthorizationCodeRequest cid scope responseType redirectUrl _state _ chal) = do
  failWithM CreateNewOAuthCodeErrorFeatureDisabled (assertMay . Opt.setOAuthEnabled <$> view settings)
  failWith CreateNewOAuthCodeErrorUnsupportedResponseType (assertMay $ responseType == OAuthResponseTypeCode)
  client <- failWithM CreateNewOAuthCodeErrorClientNotFound $ getOAuthClient uid cid
  failWith CreateNewOAuthCodeErrorRedirectUrlMissMatch (assertMay $ client.redirectUrl == redirectUrl)
  lift mkAuthorizationCode
  where
    mkAuthorizationCode :: (Handler r) OAuthAuthorizationCode
    mkAuthorizationCode = do
      oauthCode <- OAuthAuthorizationCode <$> rand32Bytes
      ttl <- Opt.setOAuthAuthorizationCodeExpirationTimeSecs <$> view settings
      lift $ wrapClient $ insertOAuthAuthorizationCode ttl oauthCode cid uid scope redirectUrl chal
      pure oauthCode

--------------------------------------------------------------------------------

createAccessTokenWith :: (Member Now r, Member Jwk r) => Either OAuthAccessTokenRequest OAuthRefreshAccessTokenRequest -> (Handler r) OAuthAccessTokenResponse
createAccessTokenWith req = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  case req of
    Left reqAC -> createAccessTokenWithAuthorizationCode reqAC
    Right reqRT -> createAccessTokenWithRefreshToken reqRT

createAccessTokenWithRefreshToken :: (Member Now r, Member Jwk r) => OAuthRefreshAccessTokenRequest -> (Handler r) OAuthAccessTokenResponse
createAccessTokenWithRefreshToken req = do
  unless (req.grantType == OAuthGrantTypeRefreshToken) $ throwStd $ errorToWai @'OAuthInvalidGrantType
  key <- signingKey
  (OAuthRefreshTokenInfo _ cid uid scope _) <- lookupVerifyAndDeleteToken key req.refreshToken
  void $ getOAuthClient uid cid >>= maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure
  unless (cid == req.clientId) $ throwStd $ errorToWai @'OAuthInvalidClientCredentials
  createAccessToken key uid cid scope

lookupVerifyAndDeleteToken :: JWK -> OAuthRefreshToken -> (Handler r) OAuthRefreshTokenInfo
lookupVerifyAndDeleteToken key =
  verifyRefreshToken key
    >=> lift . wrapClient . lookupAndDeleteOAuthRefreshToken
    >=> maybe (throwStd $ errorToWai @'OAuthInvalidRefreshToken) pure

verifyRefreshToken :: JWK -> OAuthRefreshToken -> (Handler r) OAuthRefreshTokenId
verifyRefreshToken key rt = do
  eClaims <- liftIO (OAuth.verify' key (unOAuthToken rt))
  case eClaims of
    Left _ -> throwStd $ errorToWai @'OAuthInvalidRefreshToken
    Right claims -> maybe (throwStd $ errorToWai @'OAuthInvalidRefreshToken) pure $ hcsSub claims

createAccessTokenWithAuthorizationCode :: (Member Now r, Member Jwk r) => OAuthAccessTokenRequest -> (Handler r) OAuthAccessTokenResponse
createAccessTokenWithAuthorizationCode req = do
  unless (req.grantType == OAuthGrantTypeAuthorizationCode) $ throwStd $ errorToWai @'OAuthInvalidGrantType
  (cid, uid, scope, uri, mChal) <-
    lift (wrapClient $ lookupAndDeleteByOAuthAuthorizationCode req.code)
      >>= maybe (throwStd $ errorToWai @'OAuthAuthorizationCodeNotFound) pure
  oauthClient <- getOAuthClient uid req.clientId >>= maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure

  unless (uri == req.redirectUri) $ throwStd $ errorToWai @'OAuthRedirectUrlMissMatch
  unless (oauthClient.redirectUrl == req.redirectUri) $ throwStd $ errorToWai @'OAuthRedirectUrlMissMatch
  unless (maybe False (verifyCodeChallenge req.codeVerifier) mChal) $ throwStd $ errorToWai @'OAuthInvalidGrant

  key <- signingKey
  createAccessToken key uid cid scope

signingKey :: Member Jwk r => (Handler r) JWK
signingKey = do
  fp <- view settings >>= maybe (throwStd $ errorToWai @'OAuthJwtError) pure . Opt.setOAuthJwkKeyPair
  lift (liftSem $ Jwk.get fp) >>= maybe (throwStd $ errorToWai @'OAuthJwtError) pure

createAccessToken :: (Member Now r) => JWK -> UserId -> OAuthClientId -> OAuthScopes -> (Handler r) OAuthAccessTokenResponse
createAccessToken key uid cid scope = do
  exp <- fromIntegral . Opt.setOAuthAccessTokenExpirationTimeSecs <$> view settings
  accessToken <- mkAccessToken
  (rid, refreshToken) <- mkRefreshToken
  now <- lift (liftSem Now.get)
  let refreshTokenInfo = OAuthRefreshTokenInfo rid cid uid scope now
  refreshTokenExpiration <- Opt.setOAuthRefreshTokenExpirationTimeSecs <$> view settings
  maxActiveTokens <- Opt.setOAuthMaxActiveRefreshTokens <$> view settings
  lift $ wrapClient $ insertOAuthRefreshToken maxActiveTokens refreshTokenExpiration refreshTokenInfo
  pure $ OAuthAccessTokenResponse accessToken OAuthAccessTokenTypeBearer exp refreshToken
  where
    mkRefreshToken :: (Handler r) (OAuthRefreshTokenId, OAuthRefreshToken)
    mkRefreshToken = do
      rid :: OAuthRefreshTokenId <- randomId
      sub <- maybe (throwStd $ errorToWai @'OAuthJwtError) pure $ idToText rid ^? stringOrUri
      let claims = emptyClaimsSet & claimSub ?~ sub
      (rid,) . OAuthToken <$> signRefreshToken claims

    mkAccessToken :: Member Now r => (Handler r) OAuthAccessToken
    mkAccessToken = do
      domain <- Opt.setFederationDomain <$> view settings
      exp <- fromIntegral . Opt.setOAuthAccessTokenExpirationTimeSecs <$> view settings
      claims <- mkAccessTokenClaims uid domain scope exp
      OAuthToken <$> signAccessToken claims

    mkAccessTokenClaims :: Member Now r => UserId -> Domain -> OAuthScopes -> NominalDiffTime -> (Handler r) OAuthClaimsSet
    mkAccessTokenClaims u domain scopes ttl = do
      iat <- lift (liftSem Now.get)
      uri <- maybe (throwStd $ errorToWai @'OAuthJwtError) pure $ domainText domain ^? stringOrUri
      sub <- maybe (throwStd $ errorToWai @'OAuthJwtError) pure $ idToText u ^? stringOrUri
      let exp = addUTCTime ttl iat
      let claimSet =
            emptyClaimsSet
              & claimIss ?~ uri
              & claimAud ?~ Audience [uri]
              & claimIat ?~ NumericDate iat
              & claimSub ?~ sub
              & claimExp ?~ NumericDate exp
      pure $ OAuthClaimsSet claimSet scopes

    signAccessToken :: OAuthClaimsSet -> (Handler r) SignedJWT
    signAccessToken claims = do
      jwtOrError <- liftIO $ doSignClaims
      either (const $ throwStd $ errorToWai @'OAuthJwtError) pure jwtOrError
      where
        doSignClaims :: IO (Either JWTError SignedJWT)
        doSignClaims = runJOSE $ do
          algo <- bestJWSAlg key
          signJWT key (newJWSHeader ((), algo)) claims

    signRefreshToken :: ClaimsSet -> (Handler r) SignedJWT
    signRefreshToken claims = do
      jwtOrError <- liftIO $ doSignClaims
      either (const $ throwStd $ errorToWai @'OAuthJwtError) pure jwtOrError
      where
        doSignClaims :: IO (Either JWTError SignedJWT)
        doSignClaims = runJOSE $ do
          algo <- bestJWSAlg key
          signClaims key (newJWSHeader ((), algo)) claims

--------------------------------------------------------------------------------

revokeRefreshToken :: Member Jwk r => OAuthRevokeRefreshTokenRequest -> (Handler r) ()
revokeRefreshToken req = do
  key <- signingKey
  info <- lookupAndVerifyToken key req.refreshToken
  void $ getOAuthClient info.userId info.clientId >>= maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure
  lift $ wrapClient $ deleteOAuthRefreshToken info

lookupAndVerifyToken :: JWK -> OAuthRefreshToken -> (Handler r) OAuthRefreshTokenInfo
lookupAndVerifyToken key =
  verifyRefreshToken key
    >=> lift . wrapClient . lookupOAuthRefreshTokenInfo
    >=> maybe (throwStd $ errorToWai @'OAuthInvalidRefreshToken) pure

--------------------------------------------------------------------------------

getOAuthApplications :: UserId -> (Handler r) [OAuthApplication]
getOAuthApplications uid = do
  activeRefreshTokens <- lift $ wrapClient $ lookupOAuthRefreshTokens uid
  nub . catMaybes <$> for activeRefreshTokens oauthApp
  where
    oauthApp :: OAuthRefreshTokenInfo -> (Handler r) (Maybe OAuthApplication)
    oauthApp info = (OAuthApplication info.clientId . (.name)) <$$> getOAuthClient info.userId info.clientId

--------------------------------------------------------------------------------

revokeOAuthAccountAccess :: UserId -> OAuthClientId -> (Handler r) ()
revokeOAuthAccountAccess uid cid = do
  rts <- lift $ wrapClient $ lookupOAuthRefreshTokens uid
  for_ rts $ \rt -> when (rt.clientId == cid) $ lift $ wrapClient $ deleteOAuthRefreshToken rt

--------------------------------------------------------------------------------
-- DB

deleteOAuthClient' :: (MonadClient m) => OAuthClientId -> m ()
deleteOAuthClient' cid = retry x5 . write q $ params LocalQuorum (Identity cid)
  where
    q :: PrepQuery W (Identity OAuthClientId) ()
    q = "DELETE FROM oauth_client WHERE id = ?"

updateOAuthClient' :: (MonadClient m) => OAuthClientId -> OAuthApplicationName -> RedirectUrl -> m ()
updateOAuthClient' cid name uri = retry x5 . write q $ params LocalQuorum (name, uri, cid)
  where
    q :: PrepQuery W (OAuthApplicationName, RedirectUrl, OAuthClientId) ()
    q = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE oauth_client SET name = ?, redirect_uri = ? WHERE id = ?"

insertOAuthClient :: (MonadClient m) => OAuthClientId -> OAuthApplicationName -> RedirectUrl -> Password -> m ()
insertOAuthClient cid name uri pw = retry x5 . write q $ params LocalQuorum (cid, name, uri, pw)
  where
    q :: PrepQuery W (OAuthClientId, OAuthApplicationName, RedirectUrl, Password) ()
    q = "INSERT INTO oauth_client (id, name, redirect_uri, secret) VALUES (?, ?, ?, ?)"

lookupOauthClient :: (MonadClient m) => OAuthClientId -> m (Maybe OAuthClient)
lookupOauthClient cid = do
  mNameUrl <- retry x5 . query1 q $ params LocalQuorum (Identity cid)
  pure $ mNameUrl <&> uncurry (OAuthClient cid)
  where
    q :: PrepQuery R (Identity OAuthClientId) (OAuthApplicationName, RedirectUrl)
    q = "SELECT name, redirect_uri FROM oauth_client WHERE id = ?"

insertOAuthAuthorizationCode :: (MonadClient m) => Word64 -> OAuthAuthorizationCode -> OAuthClientId -> UserId -> OAuthScopes -> RedirectUrl -> OAuthCodeChallenge -> m ()
insertOAuthAuthorizationCode ttl code cid uid scope uri chal = do
  let cqlScope = C.Set (Set.toList (unOAuthScopes scope))
  retry x5 . write q $ params LocalQuorum (code, cid, uid, cqlScope, uri, chal, fromIntegral ttl)
  where
    q :: PrepQuery W (OAuthAuthorizationCode, OAuthClientId, UserId, C.Set OAuthScope, RedirectUrl, OAuthCodeChallenge, Int32) ()
    q = fromString $ "INSERT INTO oauth_auth_code (code, client, user, scope, redirect_uri, code_challenge) VALUES (?, ?, ?, ?, ?, ?) USING TTL ?"

lookupAndDeleteByOAuthAuthorizationCode :: (MonadClient m) => OAuthAuthorizationCode -> m (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
lookupAndDeleteByOAuthAuthorizationCode code = lookupOAuthAuthorizationCode <* deleteOAuthAuthorizationCode
  where
    lookupOAuthAuthorizationCode :: (MonadClient m) => m (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
    lookupOAuthAuthorizationCode = do
      mTuple <- retry x5 . query1 q $ params LocalQuorum (Identity code)
      pure $ mTuple <&> \(cid, uid, C.Set scope, uri, mChal) -> (cid, uid, OAuthScopes (Set.fromList scope), uri, mChal)
      where
        q :: PrepQuery R (Identity OAuthAuthorizationCode) (OAuthClientId, UserId, C.Set OAuthScope, RedirectUrl, Maybe OAuthCodeChallenge)
        q = "SELECT client, user, scope, redirect_uri, code_challenge FROM oauth_auth_code WHERE code = ?"

    deleteOAuthAuthorizationCode :: (MonadClient m) => m ()
    deleteOAuthAuthorizationCode = retry x5 . write q $ params LocalQuorum (Identity code)
      where
        q :: PrepQuery W (Identity OAuthAuthorizationCode) ()
        q = "DELETE FROM oauth_auth_code WHERE code = ?"

insertOAuthRefreshToken :: (MonadClient m) => Word32 -> Word64 -> OAuthRefreshTokenInfo -> m ()
insertOAuthRefreshToken maxActiveTokens ttl info = do
  let rid = info.refreshTokenId
  oldTokes <- determineOldestTokensToBeDeleted <$> lookupOAuthRefreshTokens info.userId
  for_ oldTokes deleteOAuthRefreshToken
  retry x5 . write qInsertId $ params LocalQuorum (info.userId, rid, fromIntegral ttl)
  retry x5 . write qInsertInfo $ params LocalQuorum (rid, info.clientId, info.userId, C.Set (Set.toList (unOAuthScopes info.scopes)), info.createdAt, fromIntegral ttl)
  where
    qInsertInfo :: PrepQuery W (OAuthRefreshTokenId, OAuthClientId, UserId, C.Set OAuthScope, UTCTime, Int32) ()
    qInsertInfo = fromString $ "INSERT INTO oauth_refresh_token (id, client, user, scope, created_at) VALUES (?, ?, ?, ?, ?) USING TTL ?"

    qInsertId :: PrepQuery W (UserId, OAuthRefreshTokenId, Int32) ()
    qInsertId = fromString $ "INSERT INTO oauth_user_refresh_token (user, token_id) VALUES (?, ?) USING TTL ?"

    determineOldestTokensToBeDeleted :: [OAuthRefreshTokenInfo] -> [OAuthRefreshTokenInfo]
    determineOldestTokensToBeDeleted tokens =
      take (length sorted - fromIntegral maxActiveTokens + 1) sorted
      where
        sorted = sortOn createdAt tokens

lookupOAuthRefreshTokens :: (MonadClient m) => UserId -> m [OAuthRefreshTokenInfo]
lookupOAuthRefreshTokens uid = do
  ids <- runIdentity <$$> (retry x5 . query q $ params LocalQuorum (Identity uid))
  catMaybes <$> for ids lookupOAuthRefreshTokenInfo
  where
    q :: PrepQuery R (Identity UserId) (Identity OAuthRefreshTokenId)
    q = "SELECT token_id FROM oauth_user_refresh_token WHERE user = ?"

lookupOAuthRefreshTokenInfo :: (MonadClient m) => OAuthRefreshTokenId -> m (Maybe OAuthRefreshTokenInfo)
lookupOAuthRefreshTokenInfo rid = do
  mTuple <- retry x5 . query1 q $ params LocalQuorum (Identity rid)
  pure $ mTuple <&> \(cid, uid, C.Set scope, createdAt) -> OAuthRefreshTokenInfo rid cid uid (OAuthScopes (Set.fromList scope)) createdAt
  where
    q :: PrepQuery R (Identity OAuthRefreshTokenId) (OAuthClientId, UserId, C.Set OAuthScope, UTCTime)
    q = "SELECT client, user, scope, created_at FROM oauth_refresh_token WHERE id = ?"

deleteOAuthRefreshToken :: (MonadClient m) => OAuthRefreshTokenInfo -> m ()
deleteOAuthRefreshToken info = do
  let rid = info.refreshTokenId
  retry x5 . write qDeleteId $ params LocalQuorum (info.userId, rid)
  retry x5 . write qDeleteInfo $ params LocalQuorum (Identity rid)
  where
    qDeleteId :: PrepQuery W (UserId, OAuthRefreshTokenId) ()
    qDeleteId = "DELETE FROM oauth_user_refresh_token WHERE user = ? AND token_id = ?"

    qDeleteInfo :: PrepQuery W (Identity OAuthRefreshTokenId) ()
    qDeleteInfo = "DELETE FROM oauth_refresh_token WHERE id = ?"

lookupAndDeleteOAuthRefreshToken :: (MonadClient m) => OAuthRefreshTokenId -> m (Maybe OAuthRefreshTokenInfo)
lookupAndDeleteOAuthRefreshToken rid = do
  mInfo <- lookupOAuthRefreshTokenInfo rid
  for_ mInfo deleteOAuthRefreshToken $> mInfo
