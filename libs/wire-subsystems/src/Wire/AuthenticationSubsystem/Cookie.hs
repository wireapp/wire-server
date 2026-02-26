-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
import Wire.API.UserEvent (UserEvent (UserSessionRefreshSuggested))
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Cookie.Limit
import Wire.AuthenticationSubsystem.Error
import Wire.AuthenticationSubsystem.ZAuth
import Wire.Events
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
    Member Random r,
    Member Events r
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  SameLabelPolicy ->
  Sem r (Cookie (Token u))
newCookieImpl uid cid typ label policy = do
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
  case policy of
    KeepSameLabel -> pure ()
    RevokeSameLabel -> for_ c.cookieLabel \l -> do
      whenM (revokeCookiesMatchingExcept uid (Just c.cookieId) [] [l]) $ do
        generateUserEvent uid Nothing UserSessionRefreshSuggested
  pure c

newCookieLimitedImpl ::
  ( UserTokenLike t,
    Member SessionStore r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member (Error AuthenticationSubsystemError) r,
    Member Now r,
    Member CryptoSign r,
    Member Random r,
    Member (Error RetryAfter) r,
    Member Events r
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  SameLabelPolicy ->
  Sem r (Cookie (Token t))
newCookieLimitedImpl u c typ label policy = do
  cs <- filter ((typ ==) . cookieType) <$> SessionStore.listCookies u
  now <- Now.get
  lim <- CookieLimit <$> inputs (.userCookieLimit)
  thr <- inputs (.userCookieThrottle)
  let evict = map cookieId (limitCookies lim now cs)
  unless (null evict) $ do
    case throttleCookies now thr cs of
      Just wait -> throw wait
      Nothing -> revokeCookiesImpl u evict []
  newCookieImpl u c typ label policy

revokeCookiesImpl ::
  (Member SessionStore r) =>
  UserId ->
  [CookieId] ->
  [CookieLabel] ->
  Sem r ()
revokeCookiesImpl u ids labels = void $ revokeCookiesMatchingExcept u Nothing ids labels

revokeCookiesMatchingExcept ::
  (Member SessionStore r) =>
  UserId ->
  Maybe CookieId ->
  [CookieId] ->
  [CookieLabel] ->
  Sem r Bool
revokeCookiesMatchingExcept u Nothing [] [] = SessionStore.deleteAllCookies u $> True
revokeCookiesMatchingExcept u mself ids labels = do
  cc <- filter matching <$> SessionStore.listCookies u
  SessionStore.deleteCookies u cc
  pure $ not $ null cc
  where
    matching c =
      (Just c.cookieId /= mself)
        && ( c.cookieId `elem` ids
               || maybe False (`elem` labels) c.cookieLabel
           )
