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

module Brig.API.OAuth where

import Brig.API.Error (throwStd)
import Brig.API.Handler (Handler)
import Brig.App
import Brig.Effects.Jwk
import qualified Brig.Effects.Jwk as Jwk
import qualified Brig.Options as Opt
import Brig.Password (Password, mkSafePassword, verifyPassword)
import Cassandra hiding (Set)
import qualified Cassandra as C
import Control.Lens (view, (.~), (?~), (^?))
import Control.Monad.Except
import Crypto.JWT hiding (params, uri)
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Misc (PlainTextPassword (PlainTextPassword))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text.Ascii
import Data.Time
import Imports hiding (exp)
import OpenSSL.Random (randBytes)
import Polysemy (Member)
import Servant hiding (Handler, Tagged)
import URI.ByteString
import Wire.API.Error
import Wire.API.OAuth
import Wire.API.Routes.Internal.Brig.OAuth (IOAuthAPI)
import Wire.API.Routes.Named (Named (..))
import Wire.API.Routes.Public.Brig.OAuth (OAuthAPI)
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now

--------------------------------------------------------------------------------
-- API Internal

internalOauthAPI :: ServerT IOAuthAPI (Handler r)
internalOauthAPI =
  Named @"create-oauth-client" createNewOAuthClient

--------------------------------------------------------------------------------
-- API Public

oauthAPI :: (Member Now r, Member Jwk r) => ServerT OAuthAPI (Handler r)
oauthAPI =
  Named @"get-oauth-client" getOAuthClient
    :<|> Named @"create-oauth-auth-code" createNewOAuthAuthCode
    :<|> Named @"create-oauth-access-token" createAccessToken

--------------------------------------------------------------------------------
-- Handlers

createNewOAuthClient :: NewOAuthClient -> (Handler r) OAuthClientCredentials
createNewOAuthClient (NewOAuthClient name uri) = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  credentials@(OAuthClientCredentials cid secret) <- OAuthClientCredentials <$> randomId <*> createSecret
  safeSecret <- liftIO $ hashClientSecret secret
  lift $ wrapClient $ insertOAuthClient cid name uri safeSecret
  pure credentials
  where
    createSecret :: MonadIO m => m OAuthClientPlainTextSecret
    createSecret = OAuthClientPlainTextSecret <$> rand32Bytes

    hashClientSecret :: MonadIO m => OAuthClientPlainTextSecret -> m Password
    hashClientSecret = mkSafePassword . PlainTextPassword . toText . unOAuthClientPlainTextSecret

getOAuthClient :: UserId -> OAuthClientId -> (Handler r) (Maybe OAuthClient)
getOAuthClient _ cid = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  lift $ wrapClient $ lookupOauthClient cid

createNewOAuthAuthCode :: UserId -> NewOAuthAuthCode -> (Handler r) RedirectUrl
createNewOAuthAuthCode uid (NewOAuthAuthCode cid scope responseType redirectUrl state) = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  unless (responseType == OAuthResponseTypeCode) $ throwStd (errorToWai @'UnsupportedResponseType)
  OAuthClient _ _ uri <- getOAuthClient uid cid >>= maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure
  unless (uri == redirectUrl) $ throwStd $ errorToWai @'RedirectUrlMissMatch
  oauthCode <- OAuthAuthCode <$> rand32Bytes
  ttl <- Opt.setOAuthAuthCodeExpirationTimeSecs <$> view settings
  lift $ wrapClient $ insertOAuthAuthCode ttl oauthCode cid uid scope redirectUrl
  let queryParams = [("code", toByteString' oauthCode), ("state", cs state)]
      returnedRedirectUrl = redirectUrl & unRedirectUrl & (queryL . queryPairsL) .~ queryParams & RedirectUrl
  pure returnedRedirectUrl

createAccessToken :: (Member Now r, Member Jwk r) => OAuthAccessTokenRequest -> (Handler r) OAuthAccessTokenResponse
createAccessToken req = do
  unlessM (Opt.setOAuthEnabled <$> view settings) $ throwStd $ errorToWai @'OAuthFeatureDisabled
  (cid, uid, scope, uri) <-
    lift (wrapClient $ lookupAndDeleteOAuthAuthCode (oatCode req))
      >>= maybe (throwStd $ errorToWai @'OAuthAuthCodeNotFound) pure
  oauthClient <- getOAuthClient uid (oatClientId req) >>= maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure

  unlessM (verifyClientSecret (oatClientSecret req) (ocId oauthClient)) $ throwStd $ errorToWai @'InvalidClientCredentials
  unless (cid == oatClientId req) $ throwStd $ errorToWai @'InvalidClientCredentials
  unless (ocRedirectUrl oauthClient == oatRedirectUri req) $ throwStd $ errorToWai @'RedirectUrlMissMatch
  unless (uri == oatRedirectUri req) $ throwStd $ errorToWai @'RedirectUrlMissMatch

  exp <- fromIntegral . Opt.setOAuthAccessTokenExpirationTimeSecs <$> view settings
  fp <- view settings >>= maybe (throwStd $ errorToWai @'JwtError) pure . Opt.setOAuthJwkKeyPair
  key <- lift (liftSem $ Jwk.get fp) >>= maybe (throwStd $ errorToWai @'JwtError) pure
  accessToken <- mkAccessToken key uid scope
  (rid, refreshToken) <- mkRefreshToken key
  now <- lift (liftSem Now.get)
  let refreshTokenInfo = OAuthRefreshTokenInfo rid cid uid scope now
  let refreshTokenExpiration = 6 * 4 * 7 * 24 * 60 * 60 -- todo(leif): make this configurable (24 weeks)
  lift $ wrapClient $ insertOAuthRefreshToken refreshTokenExpiration refreshTokenInfo
  pure $ OAuthAccessTokenResponse accessToken OAuthAccessTokenTypeBearer exp refreshToken
  where
    mkRefreshToken :: (Member Now r) => JWK -> (Handler r) (OAuthRefreshTokenId, OAuthRefreshToken)
    mkRefreshToken key = do
      rid :: OAuthRefreshTokenId <- randomId
      sub <- maybe (throwStd $ errorToWai @'JwtError) pure $ idToText rid ^? stringOrUri
      let claims = emptyClaimsSet & claimSub ?~ sub
      (rid,) . OAuthToken <$> signRefreshToken key claims

    mkAccessToken :: (Member Now r, Member Jwk r) => JWK -> UserId -> OAuthScopes -> (Handler r) OAuthAccessToken
    mkAccessToken key uid scope = do
      domain <- Opt.setFederationDomain <$> view settings
      exp <- fromIntegral . Opt.setOAuthAccessTokenExpirationTimeSecs <$> view settings
      claims <- mkAccessTokenClaims uid domain scope exp
      OAuthToken <$> signAccessToken key claims

    mkAccessTokenClaims :: (Member Now r) => UserId -> Domain -> OAuthScopes -> NominalDiffTime -> (Handler r) OAuthsClaimSet
    mkAccessTokenClaims u domain scopes ttl = do
      iat <- lift (liftSem Now.get)
      uri <- maybe (throwStd $ errorToWai @'JwtError) pure $ domainText domain ^? stringOrUri
      sub <- maybe (throwStd $ errorToWai @'JwtError) pure $ idToText u ^? stringOrUri
      let exp = addUTCTime ttl iat
      let claimSet =
            emptyClaimsSet
              & claimIss ?~ uri
              & claimAud ?~ Audience [uri]
              & claimIat ?~ NumericDate iat
              & claimSub ?~ sub
              & claimExp ?~ NumericDate exp
      pure $ OAuthsClaimSet claimSet scopes

    signAccessToken :: JWK -> OAuthsClaimSet -> (Handler r) SignedJWT
    signAccessToken key claims = do
      jwtOrError <- liftIO $ doSignClaims
      either (const $ throwStd $ errorToWai @'JwtError) pure jwtOrError
      where
        doSignClaims :: IO (Either JWTError SignedJWT)
        doSignClaims = runJOSE $ do
          algo <- bestJWSAlg key
          signJWT key (newJWSHeader ((), algo)) claims

    signRefreshToken :: JWK -> ClaimsSet -> (Handler r) SignedJWT
    signRefreshToken key claims = do
      jwtOrError <- liftIO $ doSignClaims
      either (const $ throwStd $ errorToWai @'JwtError) pure jwtOrError
      where
        doSignClaims :: IO (Either JWTError SignedJWT)
        doSignClaims = runJOSE $ do
          algo <- bestJWSAlg key
          signClaims key (newJWSHeader ((), algo)) claims

    verifyClientSecret :: OAuthClientPlainTextSecret -> OAuthClientId -> (Handler r) Bool
    verifyClientSecret secret cid = do
      let plainTextPw = PlainTextPassword $ toText $ unOAuthClientPlainTextSecret secret
      lift $
        wrapClient $
          lookupOAuthClientSecret cid <&> \case
            Nothing -> False
            Just pw -> verifyPassword plainTextPw pw

rand32Bytes :: MonadIO m => m AsciiBase16
rand32Bytes = liftIO . fmap encodeBase16 $ randBytes 32

--------------------------------------------------------------------------------
-- DB

insertOAuthClient :: (MonadClient m, MonadReader Env m) => OAuthClientId -> OAuthApplicationName -> RedirectUrl -> Password -> m ()
insertOAuthClient cid name uri pw = retry x5 . write q $ params LocalQuorum (cid, name, uri, pw)
  where
    q :: PrepQuery W (OAuthClientId, OAuthApplicationName, RedirectUrl, Password) ()
    q = "INSERT INTO oauth_client (id, name, redirect_uri, secret) VALUES (?, ?, ?, ?)"

lookupOauthClient :: (MonadClient m, MonadReader Env m) => OAuthClientId -> m (Maybe OAuthClient)
lookupOauthClient cid = do
  mNameUrl <- retry x5 . query1 q $ params LocalQuorum (Identity cid)
  pure $ mNameUrl <&> uncurry (OAuthClient cid)
  where
    q :: PrepQuery R (Identity OAuthClientId) (OAuthApplicationName, RedirectUrl)
    q = "SELECT name, redirect_uri FROM oauth_client WHERE id = ?"

lookupOAuthClientSecret :: (MonadClient m, MonadReader Env m) => OAuthClientId -> m (Maybe Password)
lookupOAuthClientSecret cid = do
  runIdentity <$$> retry x5 (query1 q (params LocalQuorum (Identity cid)))
  where
    q :: PrepQuery R (Identity OAuthClientId) (Identity Password)
    q = "SELECT secret FROM oauth_client WHERE id = ?"

insertOAuthAuthCode :: (MonadClient m, MonadReader Env m) => Word64 -> OAuthAuthCode -> OAuthClientId -> UserId -> OAuthScopes -> RedirectUrl -> m ()
insertOAuthAuthCode ttl code cid uid scope uri = do
  let cqlScope = C.Set (Set.toList (unOAuthScopes scope))
  retry x5 . write q $ params LocalQuorum (code, cid, uid, cqlScope, uri)
  where
    q :: PrepQuery W (OAuthAuthCode, OAuthClientId, UserId, C.Set OAuthScope, RedirectUrl) ()
    q = fromString $ "INSERT INTO oauth_auth_code (code, client, user, scope, redirect_uri) VALUES (?, ?, ?, ?, ?) USING TTL " <> show ttl

lookupOAuthAuthCode :: (MonadClient m, MonadReader Env m) => OAuthAuthCode -> m (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl))
lookupOAuthAuthCode code = do
  mTuple <- retry x5 . query1 q $ params LocalQuorum (Identity code)
  pure $ mTuple <&> \(cid, uid, C.Set scope, uri) -> (cid, uid, OAuthScopes (Set.fromList scope), uri)
  where
    q :: PrepQuery R (Identity OAuthAuthCode) (OAuthClientId, UserId, C.Set OAuthScope, RedirectUrl)
    q = "SELECT client, user, scope, redirect_uri FROM oauth_auth_code WHERE code = ?"

deleteOAuthAuthCode :: (MonadClient m, MonadReader Env m) => OAuthAuthCode -> m ()
deleteOAuthAuthCode code = retry x5 . write q $ params LocalQuorum (Identity code)
  where
    q :: PrepQuery W (Identity OAuthAuthCode) ()
    q = "DELETE FROM oauth_auth_code WHERE code = ?"

lookupAndDeleteOAuthAuthCode :: (MonadClient m, MonadReader Env m) => OAuthAuthCode -> m (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl))
lookupAndDeleteOAuthAuthCode code = lookupOAuthAuthCode code <* deleteOAuthAuthCode code

insertOAuthRefreshToken :: (MonadClient m, MonadReader Env m) => Word64 -> OAuthRefreshTokenInfo -> m ()
insertOAuthRefreshToken ttl info = do
  let rid = oriId info
  let maxActiveTokens = 10 -- todo(leif): make this configurable
  oldTokes <- determineOldestTokensToBeDeleted maxActiveTokens <$> lookupOAuthRefreshTokens (oriUserId info)
  for_ oldTokes deleteOAuthRefreshToken
  retry x5 . write qInsertId $ params LocalQuorum (oriUserId info, rid)
  retry x5 . write qInsertInfo $ params LocalQuorum (rid, oriClientId info, oriUserId info, C.Set (Set.toList (unOAuthScopes (oriScopes info))), oriCreatedAt info)
  where
    qInsertInfo :: PrepQuery W (OAuthRefreshTokenId, OAuthClientId, UserId, C.Set OAuthScope, UTCTime) ()
    qInsertInfo = fromString $ "INSERT INTO oauth_refresh_token (id, client, user, scope, created_at) VALUES (?, ?, ?, ?, ?) USING TTL " <> show ttl

    qInsertId :: PrepQuery W (UserId, OAuthRefreshTokenId) ()
    qInsertId = fromString $ "INSERT INTO oauth_user_refresh_token (user, token_id) VALUES (?, ?) USING TTL " <> show ttl

    lookupOAuthRefreshTokenInfo :: (MonadClient m, MonadReader Env m) => OAuthRefreshTokenId -> m (Maybe OAuthRefreshTokenInfo)
    lookupOAuthRefreshTokenInfo rid = do
      mTuple <- retry x5 . query1 q $ params LocalQuorum (Identity rid)
      pure $ mTuple <&> \(cid, uid, C.Set scope, createdAt) -> OAuthRefreshTokenInfo rid cid uid (OAuthScopes (Set.fromList scope)) createdAt
      where
        q :: PrepQuery R (Identity OAuthRefreshTokenId) (OAuthClientId, UserId, C.Set OAuthScope, UTCTime)
        q = "SELECT client, user, scope, created_at FROM oauth_refresh_token WHERE id = ?"

    lookupOAuthRefreshTokens :: (MonadClient m, MonadReader Env m) => UserId -> m [OAuthRefreshTokenInfo]
    lookupOAuthRefreshTokens uid = do
      ids <- runIdentity <$$> (retry x5 . query q $ params LocalQuorum (Identity uid))
      catMaybes <$> forM ids lookupOAuthRefreshTokenInfo
      where
        q :: PrepQuery R (Identity UserId) (Identity OAuthRefreshTokenId)
        q = "SELECT token_id FROM oauth_user_refresh_token WHERE user = ?"

    determineOldestTokensToBeDeleted :: Int -> [OAuthRefreshTokenInfo] -> [OAuthRefreshTokenInfo]
    determineOldestTokensToBeDeleted maxActiveTokens tokens =
      take (length sorted - maxActiveTokens - 1) sorted
      where
        sorted = sortOn oriCreatedAt tokens

deleteOAuthRefreshToken :: (MonadClient m, MonadReader Env m) => OAuthRefreshTokenInfo -> m ()
deleteOAuthRefreshToken info = do
  let rid = oriId info
  retry x5 . write qDeleteId $ params LocalQuorum (oriUserId info, rid)
  retry x5 . write qDeleteInfo $ params LocalQuorum (Identity rid)
  where
    qDeleteId :: PrepQuery W (UserId, OAuthRefreshTokenId) ()
    qDeleteId = "DELETE FROM oauth_user_refresh_token WHERE user = ? AND token_id = ?"

    qDeleteInfo :: PrepQuery W (Identity OAuthRefreshTokenId) ()
    qDeleteInfo = "DELETE FROM oauth_refresh_token WHERE id = ?"
