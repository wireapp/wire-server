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
import Brig.API.User
import Brig.App
import Brig.Effects.BlacklistStore
import Brig.Effects.GalleyProvider
import Brig.Options
import qualified Brig.User.Auth as Auth
import Brig.ZAuth hiding (Env, settings)
import Control.Lens (view)
import Data.CommaSeparatedList
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 (List1 (..))
import Data.Qualified
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.Wai.Utilities ((!>>))
import Polysemy
import Wire.API.User
import Wire.API.User.Auth hiding (access)

accessH :: NonEmpty SomeUserToken -> Maybe SomeAccessToken -> Handler r SomeAccess
accessH ut mat = partitionTokens ut mat >>= either (uncurry access) (uncurry access)

access :: TokenPair u a => NonEmpty (Token u) -> Maybe (Token a) -> Handler r SomeAccess
access t mt =
  traverse mkUserTokenCookie
    =<< wrapHttpClientE (Auth.renewAccess (List1 t) mt) !>> zauthError

sendLoginCode :: SendLoginCode -> Handler r LoginCodeTimeout
sendLoginCode (SendLoginCode phone call force) = do
  checkWhitelist (Right phone)
  c <- wrapClientE (Auth.sendLoginCode phone call force) !>> sendLoginCodeError
  pure $ LoginCodeTimeout (pendingLoginTimeout c)

login :: Member GalleyProvider r => Login -> Maybe Bool -> Handler r SomeAccess
login l (fromMaybe False -> persist) = do
  let typ = if persist then PersistentCookie else SessionCookie
  c <- Auth.login l typ !>> loginError
  traverse mkUserTokenCookie c

logoutH :: [SomeUserToken] -> Maybe SomeAccessToken -> Handler r ()
logoutH [] Nothing = throwStd authMissingCookieAndToken
logoutH [] (Just _) = throwStd authMissingCookie
logoutH uts mat =
  partitionTokens uts mat
    >>= either (uncurry logout) (uncurry logout)

logout :: TokenPair u a => NonEmpty (Token u) -> Maybe (Token a) -> Handler r ()
logout _ Nothing = throwStd authMissingToken
logout uts (Just at) = wrapHttpClientE $ Auth.logout (List1 uts) at !>> zauthError

changeSelfEmailH ::
  Member BlacklistStore r =>
  NonEmpty SomeUserToken ->
  Maybe SomeAccessToken ->
  EmailUpdate ->
  Handler r ChangeEmailResponse
changeSelfEmailH uts mat up = do
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
  fst <$> wrapHttpClientE (Auth.validateTokens (List1 uts) mat) !>> zauthError

listCookies :: Local UserId -> Maybe (CommaSeparatedList CookieLabel) -> Handler r CookieList
listCookies lusr (fold -> labels) =
  CookieList
    <$> wrapClientE (Auth.listCookies (tUnqualified lusr) (toList labels))

removeCookies :: Local UserId -> RemoveCookies -> Handler r ()
removeCookies lusr (RemoveCookies pw lls ids) =
  wrapClientE (Auth.revokeAccess (tUnqualified lusr) pw ids lls) !>> authError

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
  Foldable f =>
  f SomeUserToken ->
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
    -- impossible
    (([], []), _) -> throwStd internalServerError
    -- mixed PlainUserToken and LHUserToken
    ((_ats, _lts), _) -> throwStd authTokenMismatch
  where
    toEither :: SomeUserToken -> Either (ZAuth.Token ZAuth.User) (ZAuth.Token ZAuth.LegalHoldUser)
    toEither (PlainUserToken ut) = Left ut
    toEither (LHUserToken lt) = Right lt
