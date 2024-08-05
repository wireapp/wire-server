-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Authorize
  ( Authorized (runAuthorized),
    authorize,
    AuthorizeUpdateEmail,
  )
where

import Control.Error hiding (bool)
import Control.Monad.Except (throwError)
import Data.Bifunctor (first, second)
import Data.Id
import Data.Kind
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 (List1 (List1))
import Data.List1 qualified as List1
import Data.Text qualified as T
import Data.ZAuth.Token qualified as ZAuth
import Imports
import Wire.API.User.Auth
import Wire.Authorize.ZAuth qualified as ZAuthWrapper

-- | Simple access control (authorization).  Allows to define functions that take arguments
-- wrapped in `Authorized`.  `Authorized` is abstract, so the only way to obtain such a value
-- is by calling one of the `Authorize` instances (see below).
--
-- This way, the application logic is not cluttered with access control code, but it is
-- guaranteed that authorization is enforced.
--
-- This is inspired by [lio](https://hackage.haskell.org/package/lio), but only controls
-- access, not information flow.
--
-- TODO: Make this a proper subsystem and use polysemy for error handling rather than either?
-- TODO: put every instance X of Authorize into a module Wire.Authorize.*.  all those modules should re-export authorize.
data Authorized op val = Authorized {runAuthorized :: val}
  deriving (Eq, Show)

-- | Different flavours of `Authorized`s.
class Authorize op where
  type AuthorizeCreds op :: Type
  type AuthorizeResult op :: Type
  type AuthorizeError op :: Type
  authorize :: AuthorizeCreds op -> Either (AuthorizeError op) (Authorized op (AuthorizeResult op))

data AuthorizeUpdateEmail

data AuthorizeUpdateEmailError
  = ParseErrorCookie Text -- \e -> Wai.mkError status403 "client-error" e
  | ParseErrorSessionToken Text -- \e -> Wai.mkError status403 "client-error" e
  | -- either ParseError.. or ParseError2.. is for backwards compatibility, details are lost in history
    ParseError2SessionToken Text -- \e -> Wai.mkError status403 "invalid-credentials" e
  | AuthTokenMismatch -- throwStd authTokenMismatch
  | AuthMissingCookieAndToken -- throwStd authMissingCookieAndToken
  | AuthMissingCookie -- throwStd authMissingCookie
  | MissingAccessToken -- throwStd missingAccessToken
  | ZAuthValidationFailure ZAuthWrapper.Failure
  deriving (Eq, Show)

instance Authorize AuthorizeUpdateEmail where
  type AuthorizeCreds AuthorizeUpdateEmail = ([Either Text SomeUserToken], Maybe (Either Text SomeAccessToken))
  type AuthorizeResult AuthorizeUpdateEmail = UserId
  type AuthorizeError AuthorizeUpdateEmail = AuthorizeUpdateEmailError

  authorize (uts', mat') = do
    uts <- handleTokenErrors uts'
    mat <- traverse handleTokenError mat'
    toks <- partitionTokens uts mat
    uid <- either (uncurry validateCredentials) (uncurry validateCredentials) toks
    Right (Authorized uid)

handleTokenErrors :: [Either Text a] -> Either AuthorizeUpdateEmailError [a]
handleTokenErrors ts = case partitionEithers ts of
  (e : _, _) -> Left (ParseErrorCookie e)
  ([], vs) -> Right vs

handleTokenError :: Either Text a -> Either AuthorizeUpdateEmailError a
handleTokenError =
  first
    ( \e ->
        if T.isPrefixOf "Failed reading" e
          then ParseErrorSessionToken e
          else ParseError2SessionToken e
    )

partitionTokens ::
  [SomeUserToken] ->
  Maybe SomeAccessToken ->
  Either
    AuthorizeUpdateEmailError
    ( Either
        (NonEmpty (ZAuth.Token ZAuth.User), Maybe (ZAuth.Token ZAuth.Access))
        (NonEmpty (ZAuth.Token ZAuth.LegalHoldUser), Maybe (ZAuth.Token ZAuth.LegalHoldAccess))
    )
partitionTokens tokens mat =
  case (partitionEithers (map toEither (toList tokens)), mat) of
    -- only PlainUserToken
    ((at : ats, []), Nothing) -> pure (Left (at :| ats, Nothing))
    ((at : ats, []), Just (PlainAccessToken a)) -> pure (Left (at :| ats, Just a))
    ((_t : _ts, []), Just (LHAccessToken _)) -> throwError AuthTokenMismatch
    -- only LHUserToken tokens
    (([], lt : lts), Nothing) -> pure (Right (lt :| lts, Nothing))
    (([], _t : _ts), Just (PlainAccessToken _)) -> throwError AuthTokenMismatch
    (([], lt : lts), Just (LHAccessToken l)) -> pure (Right (lt :| lts, Just l))
    -- no cookie
    (([], []), Nothing) -> throwError AuthMissingCookieAndToken
    (([], []), _) -> throwError AuthMissingCookie
    -- mixed PlainUserToken and LHUserToken
    ((_ats, _lts), _) -> throwError AuthTokenMismatch
  where
    toEither :: SomeUserToken -> Either (ZAuth.Token ZAuth.User) (ZAuth.Token ZAuth.LegalHoldUser)
    toEither (PlainUserToken ut) = Left ut
    toEither (LHUserToken lt) = Right lt

validateCredentials ::
  (ZAuthWrapper.TokenPair u a) =>
  NonEmpty (ZAuthWrapper.Token u) ->
  Maybe (ZAuthWrapper.Token a) ->
  Either AuthorizeUpdateEmailError UserId
validateCredentials _ Nothing = Left MissingAccessToken
validateCredentials uts mat =
  bimap ZAuthValidationFailure fst $ validateTokens (List1 uts) mat

-- | Validate a list of (User/LH) tokens potentially with an associated access token.  If
-- there are multiple valid cookies, we try all of them. When an access token is given, we
-- perform the usual checks.  If multiple cookies are given and several are valid, we return
-- the first valid one.
validateTokens ::
  forall u a v.
  (v ~ (UserId, Cookie (ZAuth.Token u))) =>
  (ZAuthWrapper.TokenPair u a) =>
  List1 (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  Either ZAuthWrapper.Failure v
validateTokens uts at = ((`validateToken` at) `mapM` uts) & second List1.head

validateToken ::
  (ZAuthWrapper.TokenPair u a) =>
  ZAuth.Token u ->
  Maybe (ZAuth.Token a) ->
  Either ZAuthWrapper.Failure (UserId, Cookie (ZAuth.Token u))
validateToken ut at = do
  unless (maybe True ((ZAuthWrapper.userTokenOf ut ==) . ZAuthWrapper.accessTokenOf) at) $
    throwError ZAuthWrapper.Invalid
  pretendWeAreInZAuthMonad $ ZAuthWrapper.validateToken @ZAuthWrapper.ZAuth ut
  case at of
    Nothing -> pure ()
    Just token ->
      case pretendWeAreInZAuthMonad (ZAuthWrapper.validateToken @ZAuthWrapper.ZAuth token) of
        Left ZAuthWrapper.Expired -> Right ()
        Left e -> Left e
        Right _ -> Right ()
  ck <- lookupCookie ut >>= maybe (Left ZAuthWrapper.Invalid) Right
  pure (ZAuthWrapper.userTokenOf ut, ck)

-- TODO: now it makes a lot more sense to turn Authorize into AuthorizationSubsystem and use
-- polysemy!
pretendWeAreInZAuthMonad :: a -> Either ZAuthWrapper.Failure ()
pretendWeAreInZAuthMonad = undefined

lookupCookie :: ZAuth.Token u -> m (Maybe (Cookie (ZAuth.Token u)))
lookupCookie = undefined

-- TODO: partitionTokens should be fine; but handleTokenErrors, handleTokenError,
-- validateTokens and validateToken in Brig.User.Auth looks *very* different, and I think it's
-- doing a subtly wrong thing.  THIS NEEDS MORE EYES!!
