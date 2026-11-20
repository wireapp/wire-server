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

-- | Activation of 'Email' addresses and 'Phone' numbers.
module Brig.Data.Activation
  ( ActivationEvent (..),
    ActivationError (..),
    activationErrorToRegisterError,
    mkActivationKey,
    activateKey,
    verifyCode,
  )
where

import Brig.App (AppT, liftSem, qualifyLocal)
import Control.Error
import Data.Id
import Data.Text.Lazy qualified as LT
import Imports
import Polysemy
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Password
import Wire.ActivationCodeStore
import Wire.ActivationCodeVerificationStore
  ( ActivationCodeVerificationStore,
    verifyActivationCode,
  )
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PasswordResetCodeStore qualified as Password
import Wire.UserKeyStore
import Wire.UserStore (UserStore)
import Wire.UserStore qualified as UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem qualified as User

data ActivationError
  = UserKeyExists !LT.Text
  | InvalidActivationCodeWrongUser
  | InvalidActivationCodeWrongCode
  | InvalidActivationEmail !EmailAddress !String
  | InvalidActivationPhone !Phone

activationErrorToRegisterError :: ActivationError -> RegisterError
activationErrorToRegisterError = \case
  UserKeyExists _ -> RegisterErrorUserKeyExists
  InvalidActivationCodeWrongUser -> RegisterErrorInvalidActivationCodeWrongUser
  InvalidActivationCodeWrongCode -> RegisterErrorInvalidActivationCodeWrongCode
  InvalidActivationEmail _ _ -> RegisterErrorInvalidEmail
  InvalidActivationPhone _ -> RegisterErrorInvalidPhone

data ActivationEvent
  = AccountActivated !User
  | EmailActivated !UserId !EmailAddress
  deriving (Show)

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  forall r.
  ( Member UserSubsystem r,
    Member PasswordResetCodeStore r,
    Member UserStore r,
    Member UserKeyStore r,
    Member ActivationCodeVerificationStore r
  ) =>
  ActivationKey ->
  ActivationCode ->
  Maybe UserId ->
  ExceptT ActivationError (AppT r) (Maybe ActivationEvent)
activateKey k c u = do
  (emailKey, mUser) <- verifyCode k c
  pickUser (emailKey, mUser) >>= activate
  where
    pickUser :: (t, Maybe UserId) -> ExceptT ActivationError (AppT r) (t, UserId)
    pickUser (uk, u') = maybe (throwE invalidUser) (pure . (uk,)) (u <|> u')

    activate :: (EmailKey, UserId) -> ExceptT ActivationError (AppT r) (Maybe ActivationEvent)
    activate (key, uid) = do
      luid <- qualifyLocal uid
      a <- lift (liftSem $ User.getAccountNoFilter luid) >>= maybe (throwE invalidUser) pure
      unless (userStatus a == Active) $ -- this is never 'PendingActivation' in the flow this function is used in.
        throwE invalidCode
      case userIdentity a of
        Nothing -> do
          claim key uid
          let ident = EmailIdentity (emailKeyOrig key)
          lift . liftSem $ UserStore.activateUser uid ident
          let a' = a {userIdentity = Just ident}
          pure . Just $ AccountActivated a'
        Just _ -> do
          let profileNeedsUpdate = Just (emailKeyOrig key) /= userEmail a
              oldKey :: Maybe EmailKey = mkEmailKey <$> userEmail a
           in handleExistingIdentity uid profileNeedsUpdate oldKey key

    handleExistingIdentity ::
      UserId ->
      Bool ->
      Maybe EmailKey ->
      EmailKey ->
      ExceptT ActivationError (AppT r) (Maybe ActivationEvent)
    handleExistingIdentity uid profileNeedsUpdate oldKey key
      | oldKey == Just key && not profileNeedsUpdate = pure Nothing
      -- activating existing key and exactly same profile
      -- (can happen when a user clicks on activation links more than once)
      | oldKey == Just key && profileNeedsUpdate = do
          lift $ updateEmailAndDeleteEmailUnvalidated uid (emailKeyOrig key)
          pure . Just $ EmailActivated uid (emailKeyOrig key)
      -- if the key is the same, we only want to update our profile
      | otherwise = do
          lift . liftSem $ Password.codeDelete (mkPasswordResetKey uid)
          claim key uid
          lift $ updateEmailAndDeleteEmailUnvalidated uid (emailKeyOrig key)
          for_ oldKey $ lift . liftSem . deleteKey
          pure . Just $ EmailActivated uid (emailKeyOrig key)
      where
        updateEmailAndDeleteEmailUnvalidated :: UserId -> EmailAddress -> AppT r ()
        updateEmailAndDeleteEmailUnvalidated u' email =
          liftSem (UserStore.updateEmail u' email <* UserStore.deleteEmailUnvalidated u')

    claim :: EmailKey -> UserId -> ExceptT ActivationError (AppT r) ()
    claim key uid = do
      ok <- lift $ liftSem (claimKey key uid)
      unless ok $
        throwE . UserKeyExists . LT.fromStrict $
          fromEmail (emailKeyOrig key)

-- | Verify an activation code via the 'ActivationCodeVerificationStore' effect.
verifyCode ::
  (Member ActivationCodeVerificationStore r) =>
  ActivationKey ->
  ActivationCode ->
  ExceptT ActivationError (AppT r) (EmailKey, Maybe UserId)
verifyCode key code = do
  mResult <- lift . liftSem $ verifyActivationCode key code
  maybe (throwE invalidCode) pure mResult

invalidUser :: ActivationError
invalidUser = InvalidActivationCodeWrongUser -- "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCodeWrongCode -- "Invalid activation code"
