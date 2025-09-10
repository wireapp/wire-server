module Wire.AuthenticationSubsystem.Cookie where

import Data.Id
import Data.RetryAfter
import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.Token
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.User.Auth
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Cookie.Limit
import Wire.AuthenticationSubsystem.Error
import Wire.AuthenticationSubsystem.ZAuth
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.SessionStore (SessionStore)
import Wire.SessionStore qualified as SessionStore

newCookieImpl ::
  ( UserTokenLike u,
    Member (Input AuthenticationSubsystemConfig) r,
    Member SessionStore r,
    Member (Error AuthenticationSubsystemError) r,
    Member Now r,
    Member CryptoSign r,
    Member Random r
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  Sem r (Cookie (Token u))
newCookieImpl uid cid typ label = do
  now <- Now.get
  tok <-
    case typ of
      PersistentCookie -> newUserToken uid cid
      SessionCookie ->
        mapError AuthenticationSubsystemZAuthFailure . fromEither
          =<< newSessionToken uid cid
  let c =
        Cookie
          { cookieId = CookieId tok.body.rand,
            cookieCreated = now,
            cookieExpires = tokenExpiresUTC tok,
            cookieLabel = label,
            cookieType = typ,
            cookieSucc = Nothing,
            cookieValue = tok
          }
  SessionStore.insertCookie uid (toUnitCookie c) Nothing
  pure c

newCookieLimitedImpl ::
  ( UserTokenLike t,
    Member SessionStore r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member (Error AuthenticationSubsystemError) r,
    Member Now r,
    Member CryptoSign r,
    Member Random r,
    Member (Error RetryAfter) r
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  Sem r (Cookie (Token t))
newCookieLimitedImpl u c typ label = do
  cs <- filter ((typ ==) . cookieType) <$> SessionStore.listCookies u
  now <- Now.get
  lim <- CookieLimit <$> inputs (.userCookieLimit)
  thr <- inputs (.userCookieThrottle)
  let evict = map cookieId (limitCookies lim now cs)
  unless (null evict) $ do
    case throttleCookies now thr cs of
      Just wait -> throw wait
      Nothing -> revokeCookiesImpl u evict []
  newCookieImpl u c typ label

revokeCookiesImpl ::
  (Member SessionStore r) =>
  UserId ->
  [CookieId] ->
  [CookieLabel] ->
  Sem r ()
revokeCookiesImpl u [] [] = SessionStore.deleteAllCookies u
revokeCookiesImpl u ids labels = do
  cc <- filter matching <$> SessionStore.listCookies u
  SessionStore.deleteCookies u cc
  where
    matching c =
      cookieId c `elem` ids
        || maybe False (`elem` labels) (cookieLabel c)
