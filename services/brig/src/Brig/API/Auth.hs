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

module Brig.API.Auth where

import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types
import Brig.App
import Brig.Options
import Brig.User.Auth qualified as Auth
import Control.Monad.Trans.Except
import Data.CommaSeparatedList
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 (List1 (..))
import Data.Qualified
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.Token (Token)
import Data.ZAuth.Token qualified as ZAuth
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities ((!>>))
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.User
import Wire.API.User.Auth hiding (access)
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Auth.Sso
import Wire.ActivationCodeStore (ActivationCodeStore)
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem qualified as Authentication
import Wire.AuthenticationSubsystem.Error (zauthError)
import Wire.AuthenticationSubsystem.ZAuth hiding (settings)
import Wire.BlockListStore
import Wire.DomainRegistrationStore (DomainRegistrationStore)
import Wire.EmailSubsystem (EmailSubsystem)
import Wire.Error (HttpError (..))
import Wire.Events (Events)
import Wire.GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.SessionStore (SessionStore)
import Wire.SparAPIAccess (SparAPIAccess)
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem (UpdateOriginType (..), UserSubsystem)
import Wire.UserSubsystem qualified as User
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.UserSubsystemConfig
import Wire.VerificationCodeSubsystem (VerificationCodeSubsystem)

accessH ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    Member (Input ZAuthEnv) r,
    Member (Input Env) r,
    Member (Embed IO) r,
    Member Metrics r,
    Member SessionStore r,
    Member (Concurrency Unsafe) r,
    Member CryptoSign r,
    Member Now r,
    Member AuthenticationSubsystem r,
    Member Random r
  ) =>
  Maybe ClientId ->
  [Either Text SomeUserToken] ->
  Maybe (Either Text SomeAccessToken) ->
  Handler r SomeAccess
accessH mcid ut' mat' = do
  ut <- handleTokenErrors ut'
  mat <- traverse handleTokenError mat'
  partitionTokens ut mat
    >>= either (uncurry (access mcid)) (uncurry (access mcid))

access ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    UserTokenLike u,
    AccessTokenLike a,
    AccessTokenType u ~ a,
    Member (Input ZAuthEnv) r,
    Member (Embed IO) r,
    Member Metrics r,
    Member SessionStore r,
    Member (Concurrency Unsafe) r,
    Member (Input Env) r,
    Member CryptoSign r,
    Member Now r,
    Member AuthenticationSubsystem r,
    Member Random r
  ) =>
  Maybe ClientId ->
  NonEmpty (Token u) ->
  Maybe (Token a) ->
  Handler r SomeAccess
access mcid t mt =
  traverse mkUserTokenCookie
    =<< Auth.renewAccess (List1 t) mt mcid !>> (StdError . zauthError)

sendLoginCode :: SendLoginCode -> Handler r LoginCodeTimeout
sendLoginCode _ =
  -- Login by phone is unsupported
  throwStd (errorToWai @'E.InvalidPhone)

login ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member UserStore r,
    Member Events r,
    Member (Input (Local ())) r,
    Member UserSubsystem r,
    Member ActivationCodeStore r,
    Member VerificationCodeSubsystem r,
    Member AuthenticationSubsystem r,
    Member (Input ZAuthEnv) r,
    Member (Input Env) r,
    Member (Concurrency Unsafe) r,
    Member SessionStore r,
    Member Now r,
    Member CryptoSign r,
    Member Random r
  ) =>
  Login ->
  Maybe Bool ->
  Handler r SomeAccess
login l (fromMaybe False -> persist) = do
  let typ = if persist then PersistentCookie else SessionCookie
  c <- Auth.login l typ !>> loginError
  traverse mkUserTokenCookie c

logoutH ::
  (Member (Input ZAuthEnv) r, Member SessionStore r, Member CryptoSign r, Member Now r) =>
  [Either Text SomeUserToken] ->
  Maybe (Either Text SomeAccessToken) ->
  Handler r ()
logoutH uts' mat' = do
  uts <- handleTokenErrors uts'
  mat <- traverse handleTokenError mat'
  partitionTokens uts mat
    >>= either (uncurry logout) (uncurry logout)

logout :: (UserTokenLike u, AccessTokenLike a, Member (Input ZAuthEnv) r, Member SessionStore r, Member CryptoSign r, Member Now r) => NonEmpty (Token u) -> Maybe (Token a) -> Handler r ()
logout _ Nothing = throwStd authMissingToken
logout uts (Just at) = Auth.logout (List1 uts) at !>> StdError . zauthError

changeSelfEmail ::
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member EmailSubsystem r,
    Member UserSubsystem r,
    Member UserStore r,
    Member ActivationCodeStore r,
    Member (Error UserSubsystemError) r,
    Member (Input UserSubsystemConfig) r,
    Member TinyLog r,
    Member DomainRegistrationStore r,
    Member SparAPIAccess r,
    Member (Input ZAuthEnv) r,
    Member CryptoSign r,
    Member Now r
  ) =>
  [Either Text SomeUserToken] ->
  Maybe (Either Text SomeAccessToken) ->
  EmailUpdate ->
  Handler r ChangeEmailResponse
changeSelfEmail uts' mat' up = do
  uts <- handleTokenErrors uts'
  mat <- traverse handleTokenError mat'
  toks <- partitionTokens uts mat
  usr <- either (uncurry validateCredentials) (uncurry validateCredentials) toks
  lusr <- qualifyLocal usr
  let email = euEmail up
  lift . liftSem $
    User.requestEmailChange lusr email UpdateOriginWireClient

validateCredentials ::
  (UserTokenLike u, AccessTokenLike a, Member (Input ZAuthEnv) r, Member CryptoSign r, Member Now r) =>
  NonEmpty (Token u) ->
  Maybe (Token a) ->
  Handler r UserId
validateCredentials _ Nothing = throwStd missingAccessToken
validateCredentials uts mat =
  fst <$> Auth.validateTokens (List1 uts) mat !>> StdError . zauthError

listCookies :: Local UserId -> Maybe (CommaSeparatedList CookieLabel) -> Handler r CookieList
listCookies lusr (fold -> labels) =
  CookieList
    <$> wrapClientE (Auth.listCookies (tUnqualified lusr) (toList labels))

removeCookies ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member AuthenticationSubsystem r,
    Member SessionStore r
  ) =>
  Local UserId ->
  RemoveCookies ->
  Handler r ()
removeCookies lusr (RemoveCookies pw lls ids) =
  Auth.revokeAccess lusr pw ids lls !>> authError

legalHoldLogin ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    Member AuthenticationSubsystem r,
    Member (Input ZAuthEnv) r,
    Member (Input Env) r,
    Member (Concurrency Unsafe) r,
    Member SessionStore r,
    Member Now r,
    Member CryptoSign r,
    Member Random r
  ) =>
  LegalHoldLogin ->
  Handler r SomeAccess
legalHoldLogin lhl = do
  let typ = PersistentCookie -- Session cookie isn't a supported use case here
  c <- Auth.legalHoldLogin lhl typ !>> legalHoldLoginError
  traverse mkUserTokenCookie c

ssoLogin ::
  ( Member TinyLog r,
    Member AuthenticationSubsystem r,
    Member UserSubsystem r,
    Member Events r,
    Member (Input ZAuthEnv) r,
    Member (Input Env) r,
    Member (Concurrency Unsafe) r,
    Member SessionStore r,
    Member Now r,
    Member CryptoSign r,
    Member Random r
  ) =>
  SsoLogin ->
  Maybe Bool ->
  Handler r SomeAccess
ssoLogin l (fromMaybe False -> persist) = do
  let typ = if persist then PersistentCookie else SessionCookie
  c <- Auth.ssoLogin l typ !>> loginError
  traverse mkUserTokenCookie c

getLoginCode :: Phone -> Handler r PendingLoginCode
getLoginCode _ = throwStd loginCodeNotFound

reauthenticate ::
  ( Member GalleyAPIAccess r,
    Member VerificationCodeSubsystem r,
    Member AuthenticationSubsystem r,
    Member UserSubsystem r
  ) =>
  Local UserId ->
  ReAuthUser ->
  Handler r ()
reauthenticate luid@(tUnqualified -> uid) body = do
  (lift . liftSem $ Authentication.reauthenticateEither uid body.reAuthPassword)
    >>= either (throwE . reauthError) (const $ pure ())
  case reAuthCodeAction body of
    Just action ->
      Auth.verifyCode (reAuthCode body) action luid
        `catchE` \case
          VerificationCodeRequired -> throwE $ reauthError ReAuthCodeVerificationRequired
          VerificationCodeNoPendingCode -> throwE $ reauthError ReAuthCodeVerificationNoPendingCode
          VerificationCodeNoEmail -> throwE $ reauthError ReAuthCodeVerificationNoEmail
    Nothing -> pure ()

--------------------------------------------------------------------------------
-- Utils

mkUserTokenCookie ::
  (MonadReader Env m, UserTokenLike u) =>
  Cookie (Token u) ->
  m UserTokenCookie
mkUserTokenCookie c = do
  s <- asks (.settings)
  pure
    UserTokenCookie
      { utcExpires =
          guard (c.cookieType == PersistentCookie)
            $> c.cookieExpires,
        utcToken = mkSomeToken c.cookieValue,
        utcSecure = not s.cookieInsecure
      }

partitionTokens ::
  [SomeUserToken] ->
  Maybe SomeAccessToken ->
  Handler
    r
    ( Either
        (NonEmpty (ZAuth.Token ZAuth.U), Maybe (ZAuth.Token ZAuth.A))
        (NonEmpty (ZAuth.Token ZAuth.LU), Maybe (ZAuth.Token ZAuth.LA))
    )
partitionTokens tokens mat =
  case (partitionEithers (map toEither (toList tokens)), mat) of
    -- only PlainUserToken
    ((at : ats, []), Nothing) -> pure (Left (at :| ats, Nothing))
    ((at : ats, []), Just (PlainAccessToken a)) -> pure (Left (at :| ats, Just a))
    ((_t : _ts, []), Just (LHAccessToken _)) -> throwStd authTokenMismatch
    -- only LHUserToken tokens
    (([], lt : lts), Nothing) -> pure (Right (lt :| lts, Nothing))
    (([], _t : _ts), Just (PlainAccessToken _)) -> throwStd authTokenMismatch
    (([], lt : lts), Just (LHAccessToken l)) -> pure (Right (lt :| lts, Just l))
    -- no cookie
    (([], []), Nothing) -> throwStd authMissingCookieAndToken
    (([], []), _) -> throwStd authMissingCookie
    -- mixed PlainUserToken and LHUserToken
    ((_ats, _lts), _) -> throwStd authTokenMismatch
  where
    toEither :: SomeUserToken -> Either (ZAuth.Token ZAuth.U) (ZAuth.Token ZAuth.LU)
    toEither (PlainUserToken ut) = Left ut
    toEither (LHUserToken lt) = Right lt

handleTokenError :: Either Text a -> Handler r a
handleTokenError =
  either
    ( \e ->
        throwStd
          . Wai.mkError status403 (label e)
          . LT.fromStrict
          $ e
    )
    pure
  where
    -- for backwards compatibility
    label e
      | T.isPrefixOf "Failed reading" e = "client-error"
      | otherwise = "invalid-credentials"

handleTokenErrors :: [Either Text a] -> Handler r [a]
handleTokenErrors ts = case partitionEithers ts of
  (e : _, []) ->
    throwStd
      . Wai.mkError status403 "client-error"
      . LT.fromStrict
      $ e
  (_, vs) -> pure vs
