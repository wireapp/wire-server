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
  ( Activation (..),
    ActivationKey (..),
    ActivationCode (..),
    ActivationEvent (..),
    ActivationError (..),
    activationErrorToRegisterError,
    newActivation,
    mkActivationKey,
    lookupActivationCode,
    activateKey,
    verifyCode,
  )
where

import Brig.App (Env)
import Brig.Data.User
import Brig.Data.UserKey
import Brig.Options
import qualified Brig.Sem.CodeStore as E
import Brig.Sem.CodeStore.Cassandra
import Brig.Types.Intra
import Cassandra
import Control.Error
import Data.Id
import Data.Text (pack)
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Imports
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
import Polysemy
import Text.Printf (printf)
import Wire.API.User
import Wire.API.User.Activation

--  | The information associated with the pending activation of a 'UserKey'.
data Activation = Activation
  { -- | An opaque key for the original 'UserKey' pending activation.
    activationKey :: !ActivationKey,
    -- | The confidential activation code.
    activationCode :: !ActivationCode
  }
  deriving (Eq, Show)

data ActivationError
  = UserKeyExists !LT.Text
  | InvalidActivationCodeWrongUser
  | InvalidActivationCodeWrongCode
  | InvalidActivationEmail !Email !String
  | InvalidActivationPhone !Phone

activationErrorToRegisterError :: ActivationError -> RegisterError
activationErrorToRegisterError = \case
  UserKeyExists _ -> RegisterErrorUserKeyExists
  InvalidActivationCodeWrongUser -> RegisterErrorInvalidActivationCodeWrongUser
  InvalidActivationCodeWrongCode -> RegisterErrorInvalidActivationCodeWrongCode
  InvalidActivationEmail _ _ -> RegisterErrorInvalidEmail
  InvalidActivationPhone _ -> RegisterErrorInvalidPhone

data ActivationEvent
  = AccountActivated !UserAccount
  | EmailActivated !UserId !Email
  | PhoneActivated !UserId !Phone

-- | Max. number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  forall m.
  (MonadClient m, MonadReader Env m) =>
  ActivationKey ->
  ActivationCode ->
  Maybe UserId ->
  ExceptT ActivationError m (Maybe ActivationEvent)
activateKey k c u = verifyCode k c >>= pickUser >>= activate
  where
    pickUser (uk, u') = maybe (throwE invalidUser) (pure . (uk,)) (u <|> u')
    activate (key, uid) = do
      a <- lift (lookupAccount uid) >>= maybe (throwE invalidUser) pure
      unless (accountStatus a == Active) $ -- this is never 'PendingActivation' in the flow this function is used in.
        throwE invalidCode
      case userIdentity (accountUser a) of
        Nothing -> do
          claim key uid
          let ident = foldKey EmailIdentity PhoneIdentity key
          lift $ activateUser uid ident
          let a' = a {accountUser = (accountUser a) {userIdentity = Just ident}}
          pure . Just $ AccountActivated a'
        Just _ -> do
          let usr = accountUser a
              (profileNeedsUpdate, oldKey) =
                foldKey
                  (\(e :: Email) -> (Just e /= userEmail usr,) . fmap userEmailKey . userEmail)
                  (\(p :: Phone) -> (Just p /= userPhone usr,) . fmap userPhoneKey . userPhone)
                  key
                  usr
           in handleExistingIdentity uid profileNeedsUpdate oldKey key
    handleExistingIdentity uid profileNeedsUpdate oldKey key
      | oldKey == Just key && not profileNeedsUpdate = pure Nothing
      -- activating existing key and exactly same profile
      -- (can happen when a user clicks on activation links more than once)
      | oldKey == Just key && profileNeedsUpdate = do
        lift $ foldKey (updateEmailAndDeleteEmailUnvalidated uid) (updatePhone uid) key
        pure . Just $ foldKey (EmailActivated uid) (PhoneActivated uid) key
      -- if the key is the same, we only want to update our profile
      | otherwise = do
        lift (runM (codeStoreToCassandra @m @'[Embed m] (E.mkPasswordResetKey uid >>= E.codeDelete)))
        claim key uid
        lift $ foldKey (updateEmailAndDeleteEmailUnvalidated uid) (updatePhone uid) key
        for_ oldKey $ lift . deleteKey
        pure . Just $ foldKey (EmailActivated uid) (PhoneActivated uid) key
      where
        updateEmailAndDeleteEmailUnvalidated :: UserId -> Email -> m ()
        updateEmailAndDeleteEmailUnvalidated u' email =
          updateEmail u' email <* deleteEmailUnvalidated u'
    claim key uid = do
      ok <- lift $ claimKey key uid
      unless ok $
        throwE . UserKeyExists . LT.fromStrict $
          foldKey fromEmail fromPhone key

-- | Create a new pending activation for a given 'UserKey'.
newActivation ::
  (MonadIO m, MonadClient m) =>
  UserKey ->
  -- | The timeout for the activation code.
  Timeout ->
  -- | The user with whom to associate the activation code.
  Maybe UserId ->
  m Activation
newActivation uk timeout u = do
  (typ, key, code) <-
    liftIO $
      foldKey
        (\e -> ("email",fromEmail e,) <$> genCode)
        (\p -> ("phone",fromPhone p,) <$> genCode)
        uk
  insert typ key code
  where
    insert t k c = do
      key <- liftIO $ mkActivationKey uk
      retry x5 . write keyInsert $ params LocalQuorum (key, t, k, c, u, maxAttempts, round timeout)
      pure $ Activation key c
    genCode =
      ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
        <$> randIntegerZeroToNMinusOne 1000000

-- | Lookup an activation code and it's associated owner (if any) for a 'UserKey'.
lookupActivationCode :: MonadClient m => UserKey -> m (Maybe (Maybe UserId, ActivationCode))
lookupActivationCode k =
  liftIO (mkActivationKey k)
    >>= retry x1 . query1 codeSelect . params LocalQuorum . Identity

-- | Verify an activation code.
verifyCode ::
  MonadClient m =>
  ActivationKey ->
  ActivationCode ->
  ExceptT ActivationError m (UserKey, Maybe UserId)
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
    mkScope "email" k u = case parseEmail k of
      Just e -> pure (userEmailKey e, u)
      Nothing -> throwE invalidCode
    mkScope "phone" k u = case parsePhone k of
      Just p -> pure (userPhoneKey p, u)
      Nothing -> throwE invalidCode
    mkScope _ _ _ = throwE invalidCode
    countdown = lift . retry x5 . write keyInsert . params LocalQuorum
    revoke = lift $ deleteActivationPair key

mkActivationKey :: UserKey -> IO ActivationKey
mkActivationKey k = do
  d <- liftIO $ getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") pure d
  let bs = digestBS d' (T.encodeUtf8 $ keyText k)
  pure . ActivationKey $ Ascii.encodeBase64Url bs

deleteActivationPair :: MonadClient m => ActivationKey -> m ()
deleteActivationPair = write keyDelete . params LocalQuorum . Identity

invalidUser :: ActivationError
invalidUser = InvalidActivationCodeWrongUser -- "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCodeWrongCode -- "Invalid activation code"

keyInsert :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
keyInsert =
  "INSERT INTO activation_keys \
  \(key, key_type, key_text, code, user, retries) VALUES \
  \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

keySelect :: PrepQuery R (Identity ActivationKey) (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)
keySelect = "SELECT ttl(code) as ttl, key_type, key_text, code, user, retries FROM activation_keys WHERE key = ?"

codeSelect :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
codeSelect = "SELECT user, code FROM activation_keys WHERE key = ?"

keyDelete :: PrepQuery W (Identity ActivationKey) ()
keyDelete = "DELETE FROM activation_keys WHERE key = ?"
