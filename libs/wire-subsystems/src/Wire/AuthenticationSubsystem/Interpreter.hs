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

module Wire.AuthenticationSubsystem.Interpreter
  ( interpretAuthenticationSubsystem,
    passwordResetCodeTtl,
    module Wire.AuthenticationSubsystem.Error,
  )
where

import Control.Error hiding (bool)
import Data.ByteString.Conversion
import Data.HavePendingInvitations
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Misc
import Data.Qualified
import Data.Time
import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.Token (Token)
import Data.ZAuth.Token qualified as ZAuth
import Data.ZAuth.Validation qualified as ZAuth
import Imports hiding (local, lookup)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger
import Util.Timeout (Timeout (..))
import Wire.API.Allowlists qualified as AllowLists
import Wire.API.User
import Wire.API.User.Auth (Cookie (..), CookieId (..), SomeAccess)
import Wire.API.User.Auth qualified as Auth
import Wire.API.User.Password
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Cookie
import Wire.AuthenticationSubsystem.Error
import Wire.AuthenticationSubsystem.ZAuth qualified as ZAuth
import Wire.ClientStore
import Wire.ClientStore qualified as Store
import Wire.EmailSubsystem
import Wire.HashPassword
import Wire.PasswordResetCodeStore
import Wire.PasswordStore (PasswordStore, upsertHashedPassword)
import Wire.PasswordStore qualified as PasswordStore
import Wire.RateLimit
import Wire.Sem.Now
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.SessionStore
import Wire.SessionStore qualified as Store
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem (UserSubsystem, getLocalAccountBy)
import Wire.UserSubsystem qualified as User

interpretAuthenticationSubsystem ::
  forall r.
  ( Member PasswordResetCodeStore r,
    Member Now r,
    Member (Error AuthenticationSubsystemError) r,
    Member TinyLog r,
    Member HashPassword r,
    Member SessionStore r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member PasswordStore r,
    Member EmailSubsystem r,
    Member UserStore r,
    Member RateLimit r,
    Member CryptoSign r,
    Member Random r,
    Member ClientStore r
  ) =>
  InterpreterFor UserSubsystem r ->
  InterpreterFor AuthenticationSubsystem r
interpretAuthenticationSubsystem userSubsystemInterpreter =
  interpret $
    userSubsystemInterpreter
      . mapError AuthenticationSubsystemZAuthFailure
      . \case
        -- Password Management
        CreatePasswordResetCode userKey -> createPasswordResetCodeImpl userKey
        ResetPassword ident resetCode newPassword -> resetPasswordImpl ident resetCode newPassword
        -- Password Verification
        AuthenticateEither uid pwd -> authenticateEitherImpl uid pwd
        ReauthenticateEither uid pwd -> reauthenticateEitherImpl uid pwd
        VerifyUserPassword uid plaintext -> verifyUserPasswordImpl uid plaintext
        VerifyUserPasswordError luid plaintext -> verifyUserPasswordErrorImpl luid plaintext
        VerifyProviderPassword pid plaintext -> verifyProviderPasswordImpl pid plaintext
        -- Cookie Management
        NewCookie uid mcid typ mLabel -> newCookieImpl uid mcid typ mLabel
        NewCookieLimited uid mcid typ mLabel -> runError $ newCookieLimitedImpl uid mcid typ mLabel
        RevokeCookies uid ids labels -> revokeCookiesImpl uid ids labels
        RotateCookie mcid rotate tokens -> rotateCookieImpl mcid rotate tokens
        ValidateTokens uts at -> validateTokensImpl uts at
        MustSuspendInactiveUser uid -> mustSuspendInactiveUserImpl uid
        -- Testing
        InternalLookupPasswordResetCode userKey -> internalLookupPasswordResetCodeImpl userKey

-- | Whether a user has not renewed any of her cookies for longer than
-- 'suspendCookiesOlderThanSecs'.  Call this always before 'newCookie', 'nextCookie',
-- 'newCookieLimited' if there is a chance that the user should be suspended (we don't do it
-- implicitly because of cyclical dependencies).
mustSuspendInactiveUserImpl ::
  ( Member SessionStore r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member Now r
  ) =>
  UserId -> Sem r Bool
mustSuspendInactiveUserImpl uid = do
  suspendInactiveUsers <- inputs (.suspendInactiveUsers)
  case suspendInactiveUsers of
    Nothing -> pure False
    Just (SuspendInactiveUsers (Timeout suspendAge)) -> do
      now <- Now.get
      let suspendHere :: UTCTime
          suspendHere = addUTCTime (-suspendAge) now
          youngEnough :: Cookie () -> Bool
          youngEnough = (>= suspendHere) . cookieCreated
      ckies <- Store.listCookies uid
      let mustSuspend
            | null ckies = False
            | any youngEnough ckies = False
            | otherwise = True
      pure mustSuspend

validateTokensImpl ::
  ( ZAuth.UserTokenLike u,
    ZAuth.AccessTokenLike a,
    Member (Input AuthenticationSubsystemConfig) r,
    Member CryptoSign r,
    Member Now r,
    Member (Error ZAuth.Failure) r,
    Member SessionStore r
  ) =>
  NonEmpty (Token u) -> Maybe (Token a) -> Sem r (UserId, Cookie (Token u))
validateTokensImpl uts at = do
  tokens <- forM uts $ \ut -> runError $ validateToken ut at
  getFirstSuccessOrFirstFail tokens
  where
    -- -- FUTUREWORK: There is surely a better way to do this
    getFirstSuccessOrFirstFail ::
      (Member (Error ZAuth.Failure) r) =>
      NonEmpty (Either ZAuth.Failure (UserId, Cookie (ZAuth.Token u))) ->
      Sem r (UserId, Cookie (ZAuth.Token u))
    getFirstSuccessOrFirstFail tks = case (lefts $ NE.toList tks, rights $ NE.toList tks) of
      (_, suc : _) -> pure suc
      (e : _, _) -> throw e
      _ -> throw ZAuth.Invalid -- Impossible
    validateToken ::
      forall r u a.
      ( ZAuth.UserTokenLike u,
        ZAuth.AccessTokenLike a,
        Member (Input AuthenticationSubsystemConfig) r,
        Member CryptoSign r,
        Member Now r,
        Member (Error ZAuth.Failure) r,
        Member SessionStore r
      ) =>
      ZAuth.Token u ->
      Maybe (ZAuth.Token a) ->
      Sem r (UserId, Cookie (ZAuth.Token u))
    validateToken ut mAt = do
      unless (maybe True ((ut.body.user ==) . (.body.userId)) mAt) $ throw ZAuth.Invalid
      either throw pure =<< ZAuth.validateToken ut
      forM_ mAt $ \token -> do
        result <- ZAuth.validateToken token
        case result of
          Left ZAuth.Expired -> throw ZAuth.Expired
          _ -> pure ()
      ck <- lookupCookieImpl ut >>= maybe (throw ZAuth.Invalid) pure
      pure (Id ut.body.user, ck)

-- | Lookup the stored cookie associated with a user token,
-- if one exists.
lookupCookieImpl ::
  ( ZAuth.Body u ~ ZAuth.User,
    Member SessionStore r
  ) =>
  ZAuth.Token u -> Sem r (Maybe (Cookie (ZAuth.Token u)))
lookupCookieImpl t = do
  let user = Id t.body.user
  let rand = t.body.rand
  let expi = ZAuth.tokenExpiresUTC t
  fmap setToken <$> Store.lookupCookie user expi (CookieId rand)
  where
    setToken c = c {cookieValue = t}

rotateCookieImpl ::
  ( Member (Input AuthenticationSubsystemConfig) r,
    Member CryptoSign r,
    Member Now r,
    Member (Error ZAuth.Failure) r,
    Member SessionStore r,
    Member ClientStore r,
    Member (Error AuthenticationSubsystemError) r,
    Member Random r,
    Member UserSubsystem r
  ) =>
  Maybe ClientId ->
  Auth.RotateCookie ->
  NonEmpty (Token ZAuth.U) ->
  Sem r SomeAccess
rotateCookieImpl mcid rotate (oldTok :| oldToks) = do
  (uid, oldCookie) <- validateTokensImpl (oldTok :| oldToks) (Nothing :: Maybe (Token ZAuth.A))
  for_ mcid $ Store.lookup uid >=> maybe (throw ZAuth.Invalid) pure
  whenM (User.suspendInactiveUser uid) $ throw ZAuth.Expired
  let oldCid = ZAuth.userTokenClient oldCookie.cookieValue.body
  when (((/=) <$> oldCid <*> mcid) == Just True) $ throw ZAuth.Invalid
  let newCid = oldCid <|> mcid
      newLabel = rotate.label <|> oldCookie.cookieLabel
  revokeCookiesImpl uid [oldCookie.cookieId] []
  rotatedCookie <- newCookieImpl @ZAuth.U uid newCid oldCookie.cookieType newLabel
  token <- ZAuth.newAccessToken rotatedCookie.cookieValue
  let accessWithCookie = Auth.Access token (Just rotatedCookie)
  insecure <- inputs (.cookieInsecure)
  pure $ ZAuth.mkUserTokenCookie insecure <$> accessWithCookie

maxAttempts :: Int32
maxAttempts = 3

passwordResetCodeTtl :: NominalDiffTime
passwordResetCodeTtl = 3600 -- 60 minutes

-- This type is not exported and used for internal control flow only
data PasswordResetError
  = AllowListError
  | InvalidResetKey
  | InProgress
  deriving (Show)

instance Exception PasswordResetError where
  displayException AllowListError = "email domain is not allowed for password reset"
  displayException InvalidResetKey = "invalid reset key for password reset"
  displayException InProgress = "password reset already in progress"

authenticateEitherImpl ::
  ( Member UserStore r,
    Member HashPassword r,
    Member PasswordStore r,
    Member RateLimit r
  ) =>
  UserId ->
  PlainTextPassword6 ->
  Sem r (Either AuthError ())
authenticateEitherImpl uid plaintext = do
  runError $
    getUserAuthenticationInfo uid >>= \case
      Nothing -> throw AuthInvalidUser
      Just (_, Deleted) -> throw AuthInvalidUser
      Just (_, Suspended) -> throw AuthSuspended
      Just (_, Ephemeral) -> throw AuthEphemeral
      Just (_, PendingInvitation) -> throw AuthPendingInvitation
      Just (Nothing, _) -> throw AuthInvalidCredentials
      Just (Just password, Active) -> do
        verifyPasswordWithStatus rateLimitKey plaintext password >>= \case
          (False, _) -> throw AuthInvalidCredentials
          (True, PasswordStatusNeedsUpdate) -> hashAndUpdatePwd plaintext
          (True, _) -> pure ()
  where
    rateLimitKey = RateLimitUser uid
    hashAndUpdatePwd pwd = do
      tryHashPassword6 rateLimitKey pwd >>= \case
        Left _ -> pure ()
        Right hashed -> upsertHashedPassword uid hashed

-- | Password reauthentication. If the account has a password, reauthentication
-- is mandatory. If
-- * User has no password, re-auth is a no-op
-- * User is an SSO user and no password is given, re-auth is a no-op.
reauthenticateEitherImpl ::
  ( Member UserStore r,
    Member UserSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  UserId ->
  Maybe (PlainTextPassword' t) ->
  Sem r (Either ReAuthError ())
reauthenticateEitherImpl user plaintextMaybe =
  getUserAuthenticationInfo user
    >>= runError
      . \case
        Nothing -> throw (ReAuthError AuthInvalidUser)
        Just (_, Deleted) -> throw (ReAuthError AuthInvalidUser)
        Just (_, Suspended) -> throw (ReAuthError AuthSuspended)
        Just (_, PendingInvitation) -> throw (ReAuthError AuthPendingInvitation)
        Just (Nothing, _) -> for_ plaintextMaybe $ const (throw $ ReAuthError AuthInvalidCredentials)
        Just (Just pw', Active) -> maybeReAuth pw'
        Just (Just pw', Ephemeral) -> maybeReAuth pw'
  where
    rateLimitKey = RateLimitUser user
    maybeReAuth pw' = case plaintextMaybe of
      Nothing -> do
        local <- inputs (.local)
        musr <- getLocalAccountBy NoPendingInvitations (qualifyAs local user)
        let isSaml = maybe False isSamlUser musr
        -- If this is a SAML user, re-auth should be no-op so no error is thrown.
        unless isSaml $
          throw ReAuthMissingPassword
      Just p ->
        unlessM (verifyPassword rateLimitKey p pw') do
          throw (ReAuthError AuthInvalidCredentials)

createPasswordResetCodeImpl ::
  forall r.
  ( Member PasswordResetCodeStore r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member Now r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member EmailSubsystem r
  ) =>
  EmailKey ->
  Sem r ()
createPasswordResetCodeImpl target =
  logPasswordResetError =<< runError do
    allowListOk <- (\e -> AllowLists.verify e (emailKeyOrig target)) <$> inputs (.allowlistEmailDomains)
    unless allowListOk $ throw AllowListError
    user <- lookupActiveUserByUserKey target >>= maybe (throw InvalidResetKey) pure
    let uid = userId user
    Log.debug $ field "user" (toByteString uid) . field "action" (val "User.beginPasswordReset")

    mExistingCode <- lookupPasswordResetCode uid
    when (isJust mExistingCode) $
      throw InProgress

    let key = mkPasswordResetKey uid
    now <- Now.get
    code <- generateEmailCode
    codeInsert
      key
      (PRQueryData code uid (Identity maxAttempts) (Identity (passwordResetCodeTtl `addUTCTime` now)))
      (round passwordResetCodeTtl)
    sendPasswordResetMail (emailKeyOrig target) (key, code) (Just user.userLocale)
    pure ()
  where
    -- `PasswordResetError` are errors that we don't want to leak to the caller.
    -- Therefore we handle them here and only log without propagating them.
    logPasswordResetError :: Either PasswordResetError () -> Sem r ()
    logPasswordResetError = \case
      Left e ->
        Log.err $
          field "action" (val "User.beginPasswordReset")
            . field "error" (displayException e)
      Right v -> pure v

lookupActiveUserIdByUserKey ::
  ( Member UserSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r
  ) =>
  EmailKey ->
  Sem r (Maybe UserId)
lookupActiveUserIdByUserKey target =
  userId <$$> lookupActiveUserByUserKey target

lookupActiveUserByUserKey ::
  ( Member UserSubsystem r,
    Member (Input AuthenticationSubsystemConfig) r
  ) =>
  EmailKey ->
  Sem r (Maybe User)
lookupActiveUserByUserKey target = do
  localUnit <- inputs (.local)
  let ltarget = qualifyAs localUnit [emailKeyOrig target]
  mUser <- User.getAccountsByEmailNoFilter ltarget
  case mUser of
    [user] -> do
      pure $
        if user.userStatus == Active
          then Just user
          else Nothing
    _ -> pure Nothing

internalLookupPasswordResetCodeImpl ::
  ( Member PasswordResetCodeStore r,
    Member Now r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member UserSubsystem r
  ) =>
  EmailKey ->
  Sem r (Maybe PasswordResetPair)
internalLookupPasswordResetCodeImpl key = do
  mUser <- lookupActiveUserIdByUserKey key
  case mUser of
    Just user -> do
      mCode <- lookupPasswordResetCode user
      let k = mkPasswordResetKey user
      pure $ (k,) <$> mCode
    Nothing -> pure Nothing

lookupPasswordResetCode ::
  ( Member PasswordResetCodeStore r,
    Member Now r
  ) =>
  UserId ->
  Sem r (Maybe PasswordResetCode)
lookupPasswordResetCode u = do
  let key = mkPasswordResetKey u
  now <- Now.get
  validate now =<< codeSelect key
  where
    validate now (Just (PRQueryData c _ _ (Just t))) | t > now = pure $ Just c
    validate _ _ = pure Nothing

resetPasswordImpl ::
  forall r.
  ( Member PasswordResetCodeStore r,
    Member Now r,
    Member (Input AuthenticationSubsystemConfig) r,
    Member (Error AuthenticationSubsystemError) r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member HashPassword r,
    Member SessionStore r,
    Member PasswordStore r,
    Member RateLimit r
  ) =>
  PasswordResetIdentity ->
  PasswordResetCode ->
  PlainTextPassword8 ->
  Sem r ()
resetPasswordImpl ident code pw = do
  key <- passwordResetKeyFromIdentity

  muid :: Maybe UserId <- verify (key, code)
  case muid of
    Nothing -> throw AuthenticationSubsystemInvalidPasswordResetCode
    Just uid -> do
      let rateLimitKey = RateLimitUser uid
      Log.debug $ field "user" (toByteString uid) . field "action" (val "User.completePasswordReset")
      checkNewIsDifferent uid pw
      hashedPw <- hashPassword8 rateLimitKey pw
      PasswordStore.upsertHashedPassword uid hashedPw
      codeDelete key
      deleteAllCookies uid
  where
    passwordResetKeyFromIdentity :: Sem r PasswordResetKey
    passwordResetKeyFromIdentity = case ident of
      PasswordResetIdentityKey k -> pure k
      PasswordResetEmailIdentity e -> do
        mUserId <- lookupActiveUserIdByUserKey (mkEmailKey e)
        let mResetKey = mkPasswordResetKey <$> mUserId
        maybe (throw AuthenticationSubsystemInvalidPasswordResetKey) pure mResetKey
      PasswordResetPhoneIdentity _ -> do
        throw AuthenticationSubsystemInvalidPhone

    checkNewIsDifferent :: UserId -> PlainTextPassword' t -> Sem r ()
    checkNewIsDifferent uid newPassword = do
      mCurrentPassword <- PasswordStore.lookupHashedPassword uid
      case mCurrentPassword of
        Just currentPassword ->
          whenM (verifyPassword (RateLimitUser uid) newPassword currentPassword) $
            throw AuthenticationSubsystemResetPasswordMustDiffer
        _ -> pure ()

    verify :: PasswordResetPair -> Sem r (Maybe UserId)
    verify (k, c) = do
      now <- Now.get
      passwordResetData <- codeSelect k
      case passwordResetData of
        Just (PRQueryData codeInDB u _ (Just t)) | c == codeInDB && t >= now -> pure (Just u)
        Just (PRQueryData codeInDB u (Just n) (Just t)) | n > 1 && t > now -> do
          -- If we only update retries, there is a chance that this races with
          -- the PasswordResetCodeTtl and we have a situation where only retries is non-null for
          -- a given key. To avoid this, we insert the whole row again.
          codeInsert k (PRQueryData codeInDB u (Identity (n - 1)) (Identity t)) (round passwordResetCodeTtl)
          pure Nothing
        Just PRQueryData {} -> codeDelete k $> Nothing
        Nothing -> pure Nothing

verifyProviderPasswordImpl ::
  ( Member PasswordStore r,
    Member (Error AuthenticationSubsystemError) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  ProviderId ->
  PlainTextPassword6 ->
  Sem r (Bool, PasswordStatus)
verifyProviderPasswordImpl pid plaintext = do
  -- We type-erase uid here
  password <-
    PasswordStore.lookupHashedProviderPassword pid
      >>= maybe (throw AuthenticationSubsystemBadCredentials) pure
  verifyPasswordWithStatus (RateLimitProvider pid) plaintext password

verifyUserPasswordImpl ::
  ( Member PasswordStore r,
    Member (Error AuthenticationSubsystemError) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  UserId ->
  PlainTextPassword6 ->
  Sem r (Bool, PasswordStatus)
verifyUserPasswordImpl uid plaintext = do
  password <-
    PasswordStore.lookupHashedPassword uid
      >>= maybe (throw AuthenticationSubsystemBadCredentials) pure
  verifyPasswordWithStatus (RateLimitUser uid) plaintext password

verifyUserPasswordErrorImpl ::
  ( Member PasswordStore r,
    Member (Error AuthenticationSubsystemError) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  Local UserId ->
  PlainTextPassword6 ->
  Sem r ()
verifyUserPasswordErrorImpl (tUnqualified -> uid) password = do
  unlessM (fst <$> verifyUserPasswordImpl uid password) do
    throw AuthenticationSubsystemBadCredentials
