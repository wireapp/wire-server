module Wire.AuthenticationSubsystem.Cookie where

import Data.Id
import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.Token
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.User.Auth
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Error
import Wire.AuthenticationSubsystem.ZAuth
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.SessionStore (SessionStore)
import Wire.SessionStore qualified as SessionStore

newCookieImpl ::
  (UserTokenLike u, Member (Input AuthenticationSubsystemConfig) r, Member SessionStore r, Member (Error AuthenticationSubsystemError) r, Member Now.Now r, Member CryptoSign r, Member Random r) =>
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
