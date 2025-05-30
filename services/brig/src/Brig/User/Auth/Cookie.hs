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
    toWebCookie,

    -- * Re-exports
    Cookie (..),
    AccessToken (..),
  )
where

import Brig.App
import Brig.Options hiding (user)
import Cassandra
import Control.Error
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.Id
import Data.List qualified as List
import Data.RetryAfter
import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.Token qualified as ZAuth
import Data.ZAuth.Validation qualified as ZAuth
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Prometheus qualified as Prom
import System.Logger.Class (field, msg, val, (~~))
import Util.Timeout
import Web.Cookie qualified as WebCookie
import Wire.API.User.Auth
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Cookie.Limit
import Wire.AuthenticationSubsystem.ZAuth qualified as ZAuth
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Metrics qualified as Metrics
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.SessionStore (SessionStore)
import Wire.SessionStore qualified as Store

--------------------------------------------------------------------------------
-- Basic Cookie Management

-- | Renew the given cookie with a fresh token, if its age
-- exceeds the configured minimum threshold.
nextCookie ::
  ( ZAuth.UserTokenLike u,
    Member TinyLog r,
    Member Metrics r,
    Member SessionStore r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member (Embed IO) r,
    Member CryptoSign r,
    Member Now r,
    Member AuthenticationSubsystem r
  ) =>
  Cookie (ZAuth.Token u) ->
  Maybe ClientId ->
  ExceptT ZAuth.Failure (Sem r) (Maybe (Cookie (ZAuth.Token u)))
nextCookie c mNewCid = runMaybeT $ do
  let mOldCid = ZAuth.userTokenClient c.cookieValue.body
  -- If both old and new client IDs are present, they must be equal
  when (((/=) <$> mOldCid <*> mNewCid) == Just True) $
    throwError ZAuth.Invalid
  -- Keep old client ID by default, but use new one if none was set.
  let mcid = mOldCid <|> mNewCid

  renewAge <- fmap fromInteger . lift . lift $ inputs (.userCookieRenewAge)
  now <- liftIO $ getCurrentTime
  let created = cookieCreated c
  -- Renew the cookie if the client ID has changed, regardless of age.
  -- FUTUREWORK: Also renew the cookie if it was signed with a different zauth
  -- key index, regardless of age.
  when (mcid == mOldCid) $ do
    guard (cookieType c == PersistentCookie)
    guard (diffUTCTime now created > renewAge)
  lift . lift $ do
    c' <- runMaybeT $ do
      ck <- hoistMaybe $ cookieSucc c
      let uid = Id c.cookieValue.body.user
      lift $ trackSuperseded uid (cookieId c)
      cs <- lift $ Store.listCookies uid
      c' <-
        hoistMaybe $
          List.find (\x -> cookieId x == ck && cookieType x == PersistentCookie) cs
      t <- lift $ ZAuth.mkUserToken uid mcid (cookieIdNum ck) (cookieExpires c')
      pure c' {cookieValue = t}
    maybe (renewCookie c mcid) pure c'

-- | Renew the given cookie with a fresh token.
renewCookie ::
  ( ZAuth.UserTokenLike u,
    Member SessionStore r,
    Member AuthenticationSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r
  ) =>
  Cookie (ZAuth.Token u) ->
  Maybe ClientId ->
  Sem r (Cookie (ZAuth.Token u))
renewCookie old mcid = do
  let t = cookieValue old
  let uid = Id t.body.user
  -- Insert new cookie
  new <- newCookie uid mcid (cookieType old) (cookieLabel old)
  -- Link the old cookie to the new (successor), keeping it
  -- around only for another renewal period so as not to build
  -- an ever growing chain of superseded cookies.
  let old' = old {cookieSucc = Just (cookieId new)}
  ttl <- inputs (.userCookieRenewAge)
  Store.insertCookie uid (toUnitCookie old') (Just (Store.TTL (fromIntegral ttl)))
  pure new

-- | Whether a user has not renewed any of her cookies for longer than
-- 'suspendCookiesOlderThanSecs'.  Call this always before 'newCookie', 'nextCookie',
-- 'newCookieLimited' if there is a chance that the user should be suspended (we don't do it
-- implicitly because of cyclical dependencies).
mustSuspendInactiveUser :: (MonadReader Env m, MonadClient m) => UserId -> m Bool
mustSuspendInactiveUser uid =
  asks (.settings.suspendInactiveUsers) >>= \case
    Nothing -> pure False
    Just (SuspendInactiveUsers (Timeout suspendAge)) -> do
      now <- liftIO =<< asks (.currentTime)
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
  forall u a r.
  ( ZAuth.UserTokenLike u,
    ZAuth.AccessTokenLike a,
    ZAuth.AccessTokenType u ~ a,
    Member (Input AuthenticationSubsystemConfig) r,
    Member CryptoSign r,
    Member Now r,
    Member Random r
  ) =>
  Cookie (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  Sem r AccessToken
newAccessToken c mt = do
  case mt of
    Nothing -> ZAuth.newAccessToken @u (cookieValue c)
    Just t -> ZAuth.renewAccessToken @a (ZAuth.userTokenClient c.cookieValue.body) t

-- | Lookup the stored cookie associated with a user token,
-- if one exists.
lookupCookie :: (ZAuth.Body u ~ ZAuth.User, MonadClient m, MonadReader Env m) => ZAuth.Token u -> m (Maybe (Cookie (ZAuth.Token u)))
lookupCookie t = do
  let user = Id t.body.user
  let rand = t.body.rand
  let expi = ZAuth.tokenExpiresUTC t
  adhocSessionStoreInterpreter $ fmap setToken <$> Store.lookupCookie user expi (CookieId rand)
  where
    setToken c = c {cookieValue = t}

listCookies :: (MonadClient m, MonadReader Env m) => UserId -> [CookieLabel] -> m [Cookie ()]
listCookies u [] = adhocSessionStoreInterpreter $ Store.listCookies u
listCookies u ll = filter byLabel <$> adhocSessionStoreInterpreter (Store.listCookies u)
  where
    byLabel c = maybe False (`elem` ll) (cookieLabel c)

revokeAllCookies :: (Member SessionStore r) => UserId -> Sem r ()
revokeAllCookies u = revokeCookies u [] []

revokeCookies :: (Member SessionStore r) => UserId -> [CookieId] -> [CookieLabel] -> Sem r ()
revokeCookies u [] [] = Store.deleteAllCookies u
revokeCookies u ids labels = do
  cc <- filter matching <$> Store.listCookies u
  Store.deleteCookies u cc
  where
    matching c =
      cookieId c `elem` ids
        || maybe False (`elem` labels) (cookieLabel c)

--------------------------------------------------------------------------------
-- Limited Cookies

newCookieLimited ::
  ( ZAuth.UserTokenLike t,
    Member SessionStore r,
    Member Now r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member AuthenticationSubsystem r
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  Sem r (Either RetryAfter (Cookie (ZAuth.Token t)))
newCookieLimited u c typ label = do
  cs <- filter ((typ ==) . cookieType) <$> Store.listCookies u
  now <- Now.get
  lim <- CookieLimit <$> inputs (.userCookieLimit)
  thr <- inputs (.userCookieThrottle)
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

toWebCookie :: (MonadReader Env m, ZAuth.UserTokenLike u) => Cookie (ZAuth.Token u) -> m WebCookie.SetCookie
toWebCookie c = do
  s <- asks (.settings)
  pure $
    WebCookie.def
      { WebCookie.setCookieName = "zuid",
        WebCookie.setCookieValue = toByteString' (cookieValue c),
        WebCookie.setCookiePath = Just "/access",
        WebCookie.setCookieExpires =
          if cookieType c == PersistentCookie
            then Just (cookieExpires c)
            else Nothing,
        WebCookie.setCookieSecure = not s.cookieInsecure,
        WebCookie.setCookieHttpOnly = True
      }

--------------------------------------------------------------------------------
-- Tracking

trackSuperseded :: (Member Metrics r, Member TinyLog r) => UserId -> CookieId -> Sem r ()
trackSuperseded u c = do
  Metrics.incCounter cookieSupersededCounter
  Log.warn $
    msg (val "Superseded cookie used")
      ~~ field "user" (toByteString u)
      ~~ field "cookie" (cookieIdNum c)

{-# NOINLINE cookieSupersededCounter #-}
cookieSupersededCounter :: Prom.Counter
cookieSupersededCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_auth_cookie_superseded",
          Prom.metricHelp = "Number of times user's cookie got superseded"
        }
