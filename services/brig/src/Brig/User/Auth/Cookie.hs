-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

    -- * Re-exports
    Cookie (..),
    AccessToken (..),
    ZAuth.UserToken,
  )
where

import Brig.App
import Brig.Options hiding (user)
import Brig.Types.User.Auth hiding (user)
import Brig.User.Auth.Cookie.Limit
import qualified Brig.User.Auth.DB.Cookie as DB
import qualified Brig.ZAuth as ZAuth
import Control.Lens (to, view)
import Data.ByteString.Conversion
import Data.Id
import qualified Data.List as List
import qualified Data.Metrics as Metrics
import Data.Proxy
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Imports
import Network.Wai (Response)
import Network.Wai.Utilities.Response (addHeader)
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import qualified Web.Cookie as WebCookie

--------------------------------------------------------------------------------
-- Basic Cookie Management

newCookie ::
  ZAuth.UserTokenLike u =>
  UserId ->
  CookieType ->
  Maybe CookieLabel ->
  AppIO (Cookie (ZAuth.Token u))
newCookie uid typ label = do
  now <- liftIO =<< view currentTime
  tok <-
    if typ == PersistentCookie
      then ZAuth.newUserToken uid
      else ZAuth.newSessionToken uid
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
  return c

-- | Renew the given cookie with a fresh token, if its age
-- exceeds the configured minimum threshold.
nextCookie :: ZAuth.UserTokenLike u => Cookie (ZAuth.Token u) -> AppIO (Maybe (Cookie (ZAuth.Token u)))
nextCookie c = do
  s <- view settings
  now <- liftIO =<< view currentTime
  let created = cookieCreated c
  let renewAge = fromInteger (setUserCookieRenewAge s)
  -- TODO: Also renew the cookie if it was signed with
  -- a different zauth key index, regardless of age.
  if persist c && diffUTCTime now created > renewAge
    then Just <$> getNext
    else return Nothing
  where
    persist = (PersistentCookie ==) . cookieType
    getNext = case cookieSucc c of
      Nothing -> renewCookie c
      Just ck -> do
        let uid = ZAuth.userTokenOf (cookieValue c)
        trackSuperseded uid (cookieId c)
        cs <- DB.listCookies uid
        case List.find (\x -> cookieId x == ck && persist x) cs of
          Nothing -> renewCookie c
          Just c' -> do
            t <- ZAuth.mkUserToken uid (cookieIdNum ck) (cookieExpires c')
            return c' {cookieValue = t}

-- | Renew the given cookie with a fresh token.
renewCookie :: ZAuth.UserTokenLike u => Cookie (ZAuth.Token u) -> AppIO (Cookie (ZAuth.Token u))
renewCookie old = do
  let t = cookieValue old
  let uid = ZAuth.userTokenOf t
  -- Insert new cookie
  new <- newCookie uid (cookieType old) (cookieLabel old)
  -- Link the old cookie to the new (successor), keeping it
  -- around only for another renewal period so as not to build
  -- an ever growing chain of superseded cookies.
  let old' = old {cookieSucc = Just (cookieId new)}
  ttl <- setUserCookieRenewAge <$> view settings
  DB.insertCookie uid old' (Just (DB.TTL (fromIntegral ttl)))
  return new

-- | Whether a user has not renewed any of her cookies for longer than
-- 'suspendCookiesOlderThanSecs'.  Call this always before 'newCookie', 'nextCookie',
-- 'newCookieLimited' if there is a chance that the user should be suspended (we don't do it
-- implicitly because of cyclical dependencies).
mustSuspendInactiveUser :: UserId -> AppIO Bool
mustSuspendInactiveUser uid =
  view (settings . to setSuspendInactiveUsers) >>= \case
    Nothing -> pure False
    Just (SuspendInactiveUsers (Timeout suspendAge)) -> do
      now <- liftIO =<< view currentTime
      let suspendHere :: UTCTime
          suspendHere = addUTCTime (- suspendAge) now
          youngEnough :: Cookie () -> Bool
          youngEnough = (>= suspendHere) . cookieCreated
      ckies <- listCookies uid []
      let mustSuspend
            | null ckies = False
            | any youngEnough ckies = False
            | otherwise = True
      pure mustSuspend

newAccessToken :: forall u a. ZAuth.TokenPair u a => Cookie (ZAuth.Token u) -> Maybe (ZAuth.Token a) -> AppIO AccessToken
newAccessToken c mt = do
  t' <- case mt of
    Nothing -> ZAuth.newAccessToken (cookieValue c)
    Just t -> ZAuth.renewAccessToken t
  zSettings <- view (zauthEnv . ZAuth.settings)
  let ttl = view (ZAuth.settingsTTL (Proxy @a)) zSettings
  return $
    bearerToken
      (ZAuth.accessTokenOf t')
      (toByteString t')
      ttl

-- | Lookup the stored cookie associated with a user token,
-- if one exists.
lookupCookie :: ZAuth.UserTokenLike u => ZAuth.Token u -> AppIO (Maybe (Cookie (ZAuth.Token u)))
lookupCookie t = do
  let user = ZAuth.userTokenOf t
  let rand = ZAuth.userTokenRand t
  let expi = ZAuth.tokenExpiresUTC t
  fmap setToken <$> DB.lookupCookie user expi (CookieId rand)
  where
    setToken c = c {cookieValue = t}

listCookies :: UserId -> [CookieLabel] -> AppIO [Cookie ()]
listCookies u [] = DB.listCookies u
listCookies u ll = filter byLabel <$> DB.listCookies u
  where
    byLabel c = maybe False (`elem` ll) (cookieLabel c)

revokeAllCookies :: UserId -> AppIO ()
revokeAllCookies u = revokeCookies u [] []

revokeCookies :: UserId -> [CookieId] -> [CookieLabel] -> AppIO ()
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
  ZAuth.UserTokenLike t =>
  UserId ->
  CookieType ->
  Maybe CookieLabel ->
  AppIO (Either RetryAfter (Cookie (ZAuth.Token t)))
newCookieLimited u typ label = do
  cs <- filter ((typ ==) . cookieType) <$> DB.listCookies u
  now <- liftIO =<< view currentTime
  lim <- CookieLimit . setUserCookieLimit <$> view settings
  thr <- setUserCookieThrottle <$> view settings
  let evict = map cookieId (limitCookies lim now cs)
  if null evict
    then Right <$> newCookie u typ label
    else case throttleCookies now thr cs of
      Just wait -> return (Left wait)
      Nothing -> do
        revokeCookies u evict []
        Right <$> newCookie u typ label

--------------------------------------------------------------------------------
-- HTTP

setResponseCookie ::
  (Monad m, MonadReader Env m, ZAuth.UserTokenLike u) =>
  Cookie (ZAuth.Token u) ->
  Response ->
  m Response
setResponseCookie c r = do
  s <- view settings
  let hdr = toByteString' (WebCookie.renderSetCookie (cookie s))
  return (addHeader "Set-Cookie" hdr r)
  where
    cookie s =
      WebCookie.def
        { WebCookie.setCookieName = "zuid",
          WebCookie.setCookieValue = toByteString' (cookieValue c),
          WebCookie.setCookieDomain = Just $ encodeUtf8 . setCookieDomain $ s,
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

trackSuperseded :: UserId -> CookieId -> AppIO ()
trackSuperseded u c = do
  m <- view metrics
  Metrics.counterIncr (Metrics.path "user.auth.cookie.superseded") m
  Log.warn $
    msg (val "Superseded cookie used")
      ~~ field "user" (toByteString u)
      ~~ field "cookie" (cookieIdNum c)
