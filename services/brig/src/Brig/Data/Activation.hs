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

import Brig.App (AppT, adhocUserKeyStoreInterpreter, liftSem, qualifyLocal, wrapClient, wrapClientE)
import Brig.Data.User
import Brig.Types.Intra
import Cassandra
import Control.Error
import Data.Id
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Imports
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
import Polysemy
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Password
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PasswordResetCodeStore qualified as Password
import Wire.UserKeyStore
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
    Member PasswordResetCodeStore r
  ) =>
  ActivationKey ->
  ActivationCode ->
  Maybe UserId ->
  ExceptT ActivationError (AppT r) (Maybe ActivationEvent)
activateKey k c u = do
  (emailKey, mUser) <- wrapClientE (verifyCode k c)
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
          wrapClientE (activateUser uid ident)
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
          for_ oldKey $ lift . adhocUserKeyStoreInterpreter . deleteKey
          pure . Just $ EmailActivated uid (emailKeyOrig key)
      where
        updateEmailAndDeleteEmailUnvalidated :: UserId -> EmailAddress -> AppT r ()
        updateEmailAndDeleteEmailUnvalidated u' email =
          wrapClient (updateEmail u' email <* deleteEmailUnvalidated u')

    claim :: EmailKey -> UserId -> ExceptT ActivationError (AppT r) ()
    claim key uid = do
      ok <- lift $ adhocUserKeyStoreInterpreter (claimKey key uid)
      unless ok $
        throwE . UserKeyExists . LT.fromStrict $
          fromEmail (emailKeyOrig key)

-- | Verify an activation code.
verifyCode ::
  (MonadClient m) =>
  ActivationKey ->
  ActivationCode ->
  ExceptT ActivationError m (EmailKey, Maybe UserId)
verifyCode key code = do
  s <- lift . retry x1 . query1 keySelect $ params LocalQuorum (Identity key)
  case s of
    Just (ttl, Ascii t, k, c, u, r) ->
      if
        | c == code -> mkScope t k u
        | r >= 1 -> countdown (key, t, k, c, u, r - 1, ttl) >> throwE invalidCode
        | otherwise -> revoke >> throwE invalidCode
    Nothing -> throwE invalidCode
  where
    mkScope "email" k u = case emailAddressText k of
      Just e -> pure (mkEmailKey e, u)
      Nothing -> throwE invalidCode
    mkScope _ _ _ = throwE invalidCode
    countdown = lift . retry x5 . write keyInsert . params LocalQuorum
    revoke = lift $ deleteActivationPair key
    keyInsert :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
    keyInsert =
      "INSERT INTO activation_keys \
      \(key, key_type, key_text, code, user, retries) VALUES \
      \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

mkActivationKey :: EmailKey -> IO ActivationKey
mkActivationKey k = do
  d <- liftIO $ getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") pure d
  let bs = digestBS d' (T.encodeUtf8 $ emailKeyUniq k)
  pure . ActivationKey $ Ascii.encodeBase64Url bs

deleteActivationPair :: (MonadClient m) => ActivationKey -> m ()
deleteActivationPair = write keyDelete . params LocalQuorum . Identity

invalidUser :: ActivationError
invalidUser = InvalidActivationCodeWrongUser -- "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCodeWrongCode -- "Invalid activation code"

keySelect :: PrepQuery R (Identity ActivationKey) (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)
keySelect = "SELECT ttl(code) as ttl, key_type, key_text, code, user, retries FROM activation_keys WHERE key = ?"

keyDelete :: PrepQuery W (Identity ActivationKey) ()
keyDelete = "DELETE FROM activation_keys WHERE key = ?"
