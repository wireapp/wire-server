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
import Brig.API.User
import Brig.App
import Brig.Data.User qualified as User
import Brig.Effects.BlacklistStore
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.GalleyProvider
import Brig.Options
import Brig.User.Auth qualified as Auth
import Brig.ZAuth hiding (Env, settings)
import Control.Lens (view)
import Control.Monad.Trans.Except
import Data.CommaSeparatedList
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 (List1 (..))
import Data.Qualified
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time.Clock (UTCTime)
import Data.ZAuth.Token qualified as ZAuth
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities ((!>>))
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Wire.API.User
import Wire.API.User.Auth hiding (access)
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Auth.Sso
import Wire.NotificationSubsystem
import Wire.Sem.Paging.Cassandra (InternalPaging)

accessH ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
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
  ( TokenPair u a,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Maybe ClientId ->
  NonEmpty (Token u) ->
  Maybe (Token a) ->
  Handler r SomeAccess
access mcid t mt =
  traverse mkUserTokenCookie
    =<< Auth.renewAccess (List1 t) mt mcid !>> zauthError

sendLoginCode :: (Member TinyLog r) => SendLoginCode -> Handler r LoginCodeTimeout
sendLoginCode (SendLoginCode phone call force) = do
  checkAllowlist (Right phone)
  c <- Auth.sendLoginCode phone call force !>> sendLoginCodeError
  pure $ LoginCodeTimeout (pendingLoginTimeout c)

login ::
  ( Member GalleyProvider r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  Login ->
  Maybe Bool ->
  Handler r SomeAccess
login l (fromMaybe False -> persist) = do
  let typ = if persist then PersistentCookie else SessionCookie
  c <- Auth.login l typ !>> loginError
  traverse mkUserTokenCookie c

logoutH ::
  [Either Text SomeUserToken] ->
  Maybe (Either Text SomeAccessToken) ->
  Handler r ()
logoutH uts' mat' = do
  uts <- handleTokenErrors uts'
  mat <- traverse handleTokenError mat'
  partitionTokens uts mat
    >>= either (uncurry logout) (uncurry logout)

logout :: TokenPair u a => NonEmpty (Token u) -> Maybe (Token a) -> Handler r ()
logout _ Nothing = throwStd authMissingToken
logout uts (Just at) = Auth.logout (List1 uts) at !>> zauthError

changeSelfEmailH ::
  Member BlacklistStore r =>
  [Either Text SomeUserToken] ->
  Maybe (Either Text SomeAccessToken) ->
  EmailUpdate ->
  Handler r ChangeEmailResponse
changeSelfEmailH uts' mat' up = do
  uts <- handleTokenErrors uts'
  mat <- traverse handleTokenError mat'
  toks <- partitionTokens uts mat
  usr <- either (uncurry validateCredentials) (uncurry validateCredentials) toks
  let email = euEmail up
  changeSelfEmail usr email ForbidSCIMUpdates

validateCredentials ::
  TokenPair u a =>
  NonEmpty (Token u) ->
  Maybe (Token a) ->
  Handler r UserId
validateCredentials _ Nothing = throwStd missingAccessToken
validateCredentials uts mat =
  fst <$> Auth.validateTokens (List1 uts) mat !>> zauthError

listCookies :: Local UserId -> Maybe (CommaSeparatedList CookieLabel) -> Handler r CookieList
listCookies lusr (fold -> labels) =
  CookieList
    <$> wrapClientE (Auth.listCookies (tUnqualified lusr) (toList labels))

removeCookies :: (Member TinyLog r) => Local UserId -> RemoveCookies -> Handler r ()
removeCookies lusr (RemoveCookies pw lls ids) =
  Auth.revokeAccess (tUnqualified lusr) pw ids lls !>> authError

legalHoldLogin ::
  ( Member GalleyProvider r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  LegalHoldLogin ->
  Handler r SomeAccess
legalHoldLogin lhl = do
  let typ = PersistentCookie -- Session cookie isn't a supported use case here
  c <- Auth.legalHoldLogin lhl typ !>> legalHoldLoginError
  traverse mkUserTokenCookie c

ssoLogin ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  SsoLogin ->
  Maybe Bool ->
  Handler r SomeAccess
ssoLogin l (fromMaybe False -> persist) = do
  let typ = if persist then PersistentCookie else SessionCookie
  c <- Auth.ssoLogin l typ !>> loginError
  traverse mkUserTokenCookie c

getLoginCode :: (Member TinyLog r) => Phone -> Handler r PendingLoginCode
getLoginCode phone = do
  code <- lift $ Auth.lookupLoginCode phone
  maybe (throwStd loginCodeNotFound) pure code

reauthenticate :: Member GalleyProvider r => UserId -> ReAuthUser -> Handler r ()
reauthenticate uid body = do
  wrapClientE (User.reauthenticate uid (reAuthPassword body)) !>> reauthError
  case reAuthCodeAction body of
    Just action ->
      Auth.verifyCode (reAuthCode body) action uid
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
  s <- view settings
  pure
    UserTokenCookie
      { utcExpires =
          guard (cookieType c == PersistentCookie)
            $> cookieExpires c,
        utcToken = mkSomeToken (cookieValue c),
        utcSecure = not (setCookieInsecure s)
      }

partitionTokens ::
  [SomeUserToken] ->
  Maybe SomeAccessToken ->
  Handler
    r
    ( Either
        (NonEmpty (ZAuth.Token ZAuth.User), Maybe (ZAuth.Token ZAuth.Access))
        (NonEmpty (ZAuth.Token ZAuth.LegalHoldUser), Maybe (ZAuth.Token ZAuth.LegalHoldAccess))
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
    toEither :: SomeUserToken -> Either (ZAuth.Token ZAuth.User) (ZAuth.Token ZAuth.LegalHoldUser)
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
