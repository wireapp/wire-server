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

module Brig.User.Auth.Cookie
  ( -- * Cookie Essentials
    newCookie,
    newAccessToken,
    nextCookie,
    lookupCookie,
    revokeCookies,
    revokeAllCookies,
    listCookies,
    mustSuspendInactiveUser,

    -- * Limited Cookies
    RetryAfter (..),
    newCookieLimited,

    -- * HTTP
    setResponseCookie,
    toWebCookie,

    -- * Re-exports
    Cookie (..),
    AccessToken (..),
  )
where

import Brig.App
import Brig.Options hiding (user)
import Brig.User.Auth.Cookie.Limit
import qualified Brig.User.Auth.DB.Cookie as DB
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Error
import Control.Lens (to, view)
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.Id
import qualified Data.List as List
import qualified Data.Metrics as Metrics
import Data.Proxy
import Data.RetryAfter
import Data.Time.Clock
import Imports
import Network.Wai (Response)
import Network.Wai.Utilities.Response (addHeader)
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import qualified Web.Cookie as WebCookie
import Wire.API.User.Auth

--------------------------------------------------------------------------------
-- Basic Cookie Management

newCookie ::
  ( ZAuth.UserTokenLike u,
    MonadReader Env m,
    ZAuth.MonadZAuth m,
    MonadClient m
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  m (Cookie (ZAuth.Token u))
newCookie uid cid typ label = do
  now <- liftIO =<< view currentTime
  tok <-
    if typ == PersistentCookie
      then ZAuth.newUserToken uid cid
      else ZAuth.newSessionToken uid cid
  let c =
        Cookie
          { cookieId = CookieId (ZAuth.userTokenRand tok),
            cookieCreated = now,
            cookieExpires = ZAuth.tokenExpiresUTC tok,
            cookieLabel = label,
            cookieType = typ,
            cookieSucc = Nothing,
            cookieValue = tok
          }
  DB.insertCookie uid c Nothing
  pure c

-- | Renew the given cookie with a fresh token, if its age
-- exceeds the configured minimum threshold.
nextCookie ::
  ( ZAuth.UserTokenLike u,
    MonadReader Env m,
    Log.MonadLogger m,
    ZAuth.MonadZAuth m,
    MonadClient m
  ) =>
  Cookie (ZAuth.Token u) ->
  Maybe ClientId ->
  ExceptT ZAuth.Failure m (Maybe (Cookie (ZAuth.Token u)))
nextCookie c mNewCid = runMaybeT $ do
  let mOldCid = ZAuth.userTokenClient (cookieValue c)
  -- If both old and new client IDs are present, they must be equal
  when (((/=) <$> mOldCid <*> mNewCid) == Just True) $
    throwError ZAuth.Invalid
  -- Keep old client ID by default, but use new one if none was set.
  let mcid = mOldCid <|> mNewCid

  s <- view settings
  now <- liftIO =<< view currentTime
  let created = cookieCreated c
  let renewAge = fromInteger (setUserCookieRenewAge s)
  -- Renew the cookie if the client ID has changed, regardless of age.
  -- FUTUREWORK: Also renew the cookie if it was signed with a different zauth
  -- key index, regardless of age.
  when (mcid == mOldCid) $ do
    guard (cookieType c == PersistentCookie)
    guard (diffUTCTime now created > renewAge)
  lift . lift $ do
    c' <- runMaybeT $ do
      ck <- hoistMaybe $ cookieSucc c
      let uid = ZAuth.userTokenOf (cookieValue c)
      lift $ trackSuperseded uid (cookieId c)
      cs <- lift $ DB.listCookies uid
      c' <-
        hoistMaybe $
          List.find (\x -> cookieId x == ck && cookieType x == PersistentCookie) cs
      t <- lift $ ZAuth.mkUserToken uid mcid (cookieIdNum ck) (cookieExpires c')
      pure c' {cookieValue = t}
    maybe (renewCookie c mcid) pure c'

-- | Renew the given cookie with a fresh token.
renewCookie ::
  ( ZAuth.UserTokenLike u,
    MonadReader Env m,
    ZAuth.MonadZAuth m,
    MonadClient m
  ) =>
  Cookie (ZAuth.Token u) ->
  Maybe ClientId ->
  m (Cookie (ZAuth.Token u))
renewCookie old mcid = do
  let t = cookieValue old
  let uid = ZAuth.userTokenOf t
  -- Insert new cookie
  new <- newCookie uid mcid (cookieType old) (cookieLabel old)
  -- Link the old cookie to the new (successor), keeping it
  -- around only for another renewal period so as not to build
  -- an ever growing chain of superseded cookies.
  let old' = old {cookieSucc = Just (cookieId new)}
  ttl <- setUserCookieRenewAge <$> view settings
  DB.insertCookie uid old' (Just (DB.TTL (fromIntegral ttl)))
  pure new

-- | Whether a user has not renewed any of her cookies for longer than
-- 'suspendCookiesOlderThanSecs'.  Call this always before 'newCookie', 'nextCookie',
-- 'newCookieLimited' if there is a chance that the user should be suspended (we don't do it
-- implicitly because of cyclical dependencies).
mustSuspendInactiveUser :: (MonadReader Env m, MonadClient m) => UserId -> m Bool
mustSuspendInactiveUser uid =
  view (settings . to setSuspendInactiveUsers) >>= \case
    Nothing -> pure False
    Just (SuspendInactiveUsers (Timeout suspendAge)) -> do
      now <- liftIO =<< view currentTime
      let suspendHere :: UTCTime
          suspendHere = addUTCTime (-suspendAge) now
          youngEnough :: Cookie () -> Bool
          youngEnough = (>= suspendHere) . cookieCreated
      ckies <- listCookies uid []
      let mustSuspend
            | null ckies = False
            | any youngEnough ckies = False
            | otherwise = True
      pure mustSuspend

newAccessToken ::
  forall u a m.
  (ZAuth.TokenPair u a, MonadReader Env m, ZAuth.MonadZAuth m) =>
  Cookie (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  m AccessToken
newAccessToken c mt = do
  t' <- case mt of
    Nothing -> ZAuth.newAccessToken (cookieValue c)
    Just t -> ZAuth.renewAccessToken (ZAuth.userTokenClient (cookieValue c)) t
  zSettings <- view (zauthEnv . ZAuth.settings)
  let ttl = view (ZAuth.settingsTTL (Proxy @a)) zSettings
  pure $
    bearerToken
      (ZAuth.accessTokenOf t')
      (toByteString t')
      ttl

-- | Lookup the stored cookie associated with a user token,
-- if one exists.
lookupCookie :: (ZAuth.UserTokenLike u, MonadClient m) => ZAuth.Token u -> m (Maybe (Cookie (ZAuth.Token u)))
lookupCookie t = do
  let user = ZAuth.userTokenOf t
  let rand = ZAuth.userTokenRand t
  let expi = ZAuth.tokenExpiresUTC t
  fmap setToken <$> DB.lookupCookie user expi (CookieId rand)
  where
    setToken c = c {cookieValue = t}

listCookies :: MonadClient m => UserId -> [CookieLabel] -> m [Cookie ()]
listCookies u [] = DB.listCookies u
listCookies u ll = filter byLabel <$> DB.listCookies u
  where
    byLabel c = maybe False (`elem` ll) (cookieLabel c)

revokeAllCookies :: MonadClient m => UserId -> m ()
revokeAllCookies u = revokeCookies u [] []

revokeCookies :: MonadClient m => UserId -> [CookieId] -> [CookieLabel] -> m ()
revokeCookies u [] [] = DB.deleteAllCookies u
revokeCookies u ids labels = do
  cc <- filter matching <$> DB.listCookies u
  DB.deleteCookies u cc
  where
    matching c =
      cookieId c `elem` ids
        || maybe False (`elem` labels) (cookieLabel c)

--------------------------------------------------------------------------------
-- Limited Cookies

newCookieLimited ::
  ( ZAuth.UserTokenLike t,
    MonadReader Env m,
    MonadClient m,
    ZAuth.MonadZAuth m
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  m (Either RetryAfter (Cookie (ZAuth.Token t)))
newCookieLimited u c typ label = do
  cs <- filter ((typ ==) . cookieType) <$> DB.listCookies u
  now <- liftIO =<< view currentTime
  lim <- CookieLimit . setUserCookieLimit <$> view settings
  thr <- setUserCookieThrottle <$> view settings
  let evict = map cookieId (limitCookies lim now cs)
  if null evict
    then Right <$> newCookie u c typ label
    else case throttleCookies now thr cs of
      Just wait -> pure (Left wait)
      Nothing -> do
        revokeCookies u evict []
        Right <$> newCookie u c typ label

--------------------------------------------------------------------------------
-- HTTP

setResponseCookie ::
  (Monad m, MonadReader Env m, ZAuth.UserTokenLike u) =>
  Cookie (ZAuth.Token u) ->
  Response ->
  m Response
setResponseCookie c r = do
  hdr <- toByteString' . WebCookie.renderSetCookie <$> toWebCookie c
  pure (addHeader "Set-Cookie" hdr r)

toWebCookie :: (Monad m, MonadReader Env m, ZAuth.UserTokenLike u) => Cookie (ZAuth.Token u) -> m WebCookie.SetCookie
toWebCookie c = do
  s <- view settings
  pure $
    WebCookie.def
      { WebCookie.setCookieName = "zuid",
        WebCookie.setCookieValue = toByteString' (cookieValue c),
        WebCookie.setCookiePath = Just "/access",
        WebCookie.setCookieExpires =
          if cookieType c == PersistentCookie
            then Just (cookieExpires c)
            else Nothing,
        WebCookie.setCookieSecure = not (setCookieInsecure s),
        WebCookie.setCookieHttpOnly = True
      }

--------------------------------------------------------------------------------
-- Tracking

trackSuperseded :: (MonadReader Env m, MonadIO m, Log.MonadLogger m) => UserId -> CookieId -> m ()
trackSuperseded u c = do
  m <- view metrics
  Metrics.counterIncr (Metrics.path "user.auth.cookie.superseded") m
  Log.warn $
    msg (val "Superseded cookie used")
      ~~ field "user" (toByteString u)
      ~~ field "cookie" (cookieIdNum c)
