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
    module Wire.AuthenticationSubsystem.Error,
  )
where

import Data.ByteString.Conversion
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Time
import Imports hiding (lookup)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger
import Wire.API.Allowlists (AllowlistEmailDomains, AllowlistPhonePrefixes)
import Wire.API.Allowlists qualified as AllowLists
import Wire.API.Password
import Wire.API.User
import Wire.API.User.Password
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Error
import Wire.HashPassword
import Wire.PasswordResetCodeStore
import Wire.PasswordStore
import Wire.Sem.Now
import Wire.Sem.Now qualified as Now
import Wire.SessionStore
import Wire.UserKeyStore
import Wire.UserSubsystem (UserSubsystem, getLocalUserAccountByUserKey)

interpretAuthenticationSubsystem ::
  forall r.
  ( Member PasswordResetCodeStore r,
    Member Now r,
    Member (Error AuthenticationSubsystemError) r,
    Member TinyLog r,
    Member HashPassword r,
    Member SessionStore r,
    Member (Input (Local ())) r,
    Member (Input (Maybe AllowlistEmailDomains)) r,
    Member (Input (Maybe AllowlistPhonePrefixes)) r,
    Member UserSubsystem r,
    Member PasswordStore r
  ) =>
  InterpreterFor AuthenticationSubsystem r
interpretAuthenticationSubsystem = interpret $ \case
  CreatePasswordResetCode userKey -> createPasswordResetCodeImpl userKey
  ResetPassword ident resetCode newPassword -> resetPasswordImpl ident resetCode newPassword
  InternalLookupPasswordResetCode userKey -> internalLookupPasswordResetCodeImpl userKey

maxAttempts :: Int32
maxAttempts = 3

ttl :: NominalDiffTime
ttl = 3600 -- 60 minutes

createPasswordResetCodeImpl ::
  ( Member PasswordResetCodeStore r,
    Member Now r,
    Member (Input (Local ())) r,
    Member (Input (Maybe AllowlistEmailDomains)) r,
    Member (Input (Maybe AllowlistPhonePrefixes)) r,
    Member (Error AuthenticationSubsystemError) r,
    Member TinyLog r,
    Member UserSubsystem r
  ) =>
  UserKey ->
  Sem r (UserId, PasswordResetPair)
createPasswordResetCodeImpl target = do
  allowListOk <- (\e p -> AllowLists.verify e p (toEither target)) <$> input <*> input
  unless allowListOk $ throw AuthenticationSubsystemAllowListError
  user <- lookupActiveUserIdByUserKey target >>= maybe (throw AuthenticationSubsystemInvalidPasswordResetKey) pure
  Log.debug $ field "user" (toByteString user) . field "action" (val "User.beginPasswordReset")

  mExistingCode <- lookupPasswordResetCode user
  when (isJust mExistingCode) $
    throw AuthenticationSubsystemPasswordResetInProgress

  let key = mkPasswordResetKey user
  now <- Now.get
  code <- foldKey (const generateEmailCode) (const generatePhoneCode) target
  codeInsert
    key
    (PRQueryData code user (Identity maxAttempts) (Identity (ttl `addUTCTime` now)))
    (round ttl)
  pure (user, (key, code))

lookupActiveUserIdByUserKey :: (Member UserSubsystem r, Member (Input (Local ())) r) => UserKey -> Sem r (Maybe UserId)
lookupActiveUserIdByUserKey target = do
  localUnit <- input
  let ltarget = qualifyAs localUnit target
  mUser <- getLocalUserAccountByUserKey ltarget
  case mUser of
    Just user -> do
      pure $
        if user.accountStatus == Active
          then Just $ userId user.accountUser
          else Nothing
    Nothing -> pure Nothing

internalLookupPasswordResetCodeImpl ::
  ( Member PasswordResetCodeStore r,
    Member Now r,
    Member (Input (Local ())) r,
    Member UserSubsystem r
  ) =>
  UserKey ->
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
    Member (Input (Local ())) r,
    Member (Error AuthenticationSubsystemError) r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member HashPassword r,
    Member SessionStore r,
    Member PasswordStore r
  ) =>
  PasswordResetIdentity ->
  PasswordResetCode ->
  PlainTextPassword8 ->
  Sem r ()
resetPasswordImpl ident code pw = do
  key <- passwordResetKeyFromIdentity

  muid :: Maybe UserId <- verify (key, code)
  case muid of
    Nothing -> throw AuthenticationSubSystemInvalidPasswordResetCode
    Just uid -> do
      Log.debug $ field "user" (toByteString uid) . field "action" (val "User.completePasswordReset")
      checkNewIsDifferent uid pw
      hashedPw <- hashPasswordArgon2id pw
      upsertHashedPassword uid hashedPw
      codeDelete key
      deleteAllCookies uid
  where
    passwordResetKeyFromIdentity :: Sem r PasswordResetKey
    passwordResetKeyFromIdentity = case ident of
      PasswordResetIdentityKey k -> pure k
      PasswordResetEmailIdentity e -> do
        mUserId <- lookupActiveUserIdByUserKey (userEmailKey e)
        let mResetKey = mkPasswordResetKey <$> mUserId
        maybe (throw AuthenticationSubsystemInvalidPasswordResetKey) pure mResetKey
      PasswordResetPhoneIdentity p -> do
        mUserId <- lookupActiveUserIdByUserKey (userPhoneKey p)
        let mResetKey = mkPasswordResetKey <$> mUserId
        maybe (throw AuthenticationSubsystemInvalidPasswordResetKey) pure mResetKey

    checkNewIsDifferent :: UserId -> PlainTextPassword' t -> Sem r ()
    checkNewIsDifferent uid newPassword = do
      mCurrentPassword <- lookupHashedPassword uid
      case mCurrentPassword of
        Just currentPassword
          | (verifyPassword newPassword currentPassword) -> throw AuthenticationSubsystemResetPasswordMustDiffer
        _ -> pure ()

    verify :: PasswordResetPair -> Sem r (Maybe UserId)
    verify (k, c) = do
      now <- Now.get
      passwordResetData <- codeSelect k
      case passwordResetData of
        Just (PRQueryData codeInDB u _ (Just t)) | c == codeInDB && t >= now -> pure (Just u)
        Just (PRQueryData codeInDB u (Just n) (Just t)) | n > 1 && t > now -> do
          -- If we only update retries, there is a chance that this races with
          -- the TTL and we have a situation where only retries is non-null for
          -- a given key. To avoid this, we insert the whole row again.
          codeInsert k (PRQueryData codeInDB u (Identity (n - 1)) (Identity t)) (round ttl)
          pure Nothing
        Just PRQueryData {} -> codeDelete k $> Nothing
        Nothing -> pure Nothing
