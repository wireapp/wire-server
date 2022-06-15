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

    -- * polysemized version of 'mkActivationKey'
    makeActivationKey,
  )
where

import Brig.Data.User
import Brig.Data.UserKey
import Brig.Options
import Brig.Sem.ActivationKeyStore
import Brig.Sem.ActivationSupply
import Brig.Sem.PasswordResetStore
import qualified Brig.Sem.PasswordResetStore as E
import qualified Brig.Sem.PasswordResetSupply as E
import Brig.Sem.UserKeyStore (UserKeyStore)
import Brig.Sem.UserQuery (UserQuery)
import Brig.Types
import Brig.Types.Intra
import Cassandra
import Control.Error
import Data.Id
import Data.Qualified
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Imports
import OpenSSL.EVP.Digest (Digest, digestBS, getDigestByName)
import Polysemy
import Polysemy.Input
import Wire.API.User

--  | The information associated with the pending activation of a 'UserKey'.
data Activation = Activation
  { -- | An opaque key for the original 'UserKey' pending activation.
    activationKey :: !ActivationKey,
    -- | The confidential activation code.
    activationCode :: !ActivationCode
  }
  deriving (Eq)

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

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  forall r.
  Members
    '[ ActivationKeyStore,
       Input (Local ()),
       E.PasswordResetSupply,
       PasswordResetStore,
       UserKeyStore,
       UserQuery
     ]
    r =>
  Locale ->
  Digest ->
  ActivationKey ->
  ActivationCode ->
  Maybe UserId ->
  ExceptT ActivationError (Sem r) (Maybe ActivationEvent)
activateKey locale d k c u = verifyCode k c >>= pickUser >>= activate
  where
    pickUser (uk, u') = maybe (throwE invalidUser) (pure . (uk,)) (u <|> u')
    activate (key, uid) = do
      a <-
        lift (lookupAccount locale uid) >>= maybe (throwE invalidUser) pure
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
        lift (deleteCode uid)
        claim key uid
        lift $ foldKey (updateEmailAndDeleteEmailUnvalidated uid) (updatePhone uid) key
        for_ oldKey $ lift . deleteKey d
        pure . Just $ foldKey (EmailActivated uid) (PhoneActivated uid) key
      where
        updateEmailAndDeleteEmailUnvalidated :: UserId -> Email -> Sem r ()
        updateEmailAndDeleteEmailUnvalidated u' email =
          updateEmail u' email <* deleteEmailUnvalidated u'
        deleteCode :: UserId -> Sem r ()
        deleteCode uId =
          -- FUTUREWORK: use the DeletePasswordResetCode action instead of CodeDelete
          E.mkPasswordResetKey uId
            >>= E.deletePasswordResetCode
    claim key uid = do
      ok <- lift $ claimKey d key uid
      unless ok $
        throwE . UserKeyExists . LT.fromStrict $
          foldKey fromEmail fromPhone key

-- | Create a new pending activation for a given 'UserKey'.
newActivation ::
  Members '[ActivationKeyStore, ActivationSupply] r =>
  UserKey ->
  -- | The timeout for the activation code.
  Timeout ->
  -- | The user with whom to associate the activation code.
  Maybe UserId ->
  Sem r Activation
newActivation uk timeout u = do
  (typ, key, code) <-
    foldKey
      (\e -> ("email",fromEmail e,) <$> makeActivationCode)
      (\p -> ("phone",fromPhone p,) <$> makeActivationCode)
      uk
  insert typ key code
  where
    insert t k c = do
      key <- makeActivationKey uk
      insertActivationKey (key, t, k, c, u, maxAttempts, round timeout)
      pure $ Activation key c

-- | Lookup an activation code and it's associated owner (if any) for a 'UserKey'.
lookupActivationCode :: MonadClient m => UserKey -> m (Maybe (Maybe UserId, ActivationCode))
lookupActivationCode k =
  liftIO (mkActivationKey k)
    >>= retry x1 . query1 codeSelect . params LocalQuorum . Identity

-- | Verify an activation code.
verifyCode ::
  Members '[ActivationKeyStore] r =>
  ActivationKey ->
  ActivationCode ->
  ExceptT ActivationError (Sem r) (UserKey, Maybe UserId)
verifyCode key code = do
  s <- lift . getActivationKey $ key
  case s of
    Just (ttl, Ascii t, k, c, u, r) ->
      if
          | c == code -> mkScope t k u
          | r >= 1 -> countdown (key, t, k, c, u, r -1, ttl) >> throwE invalidCode
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
    countdown = lift . insertActivationKey
    revoke = lift $ deleteActivationPair key

mkActivationKey :: UserKey -> IO ActivationKey
mkActivationKey k = do
  d <- liftIO $ getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") pure d
  let bs = digestBS d' (T.encodeUtf8 $ keyText k)
  pure . ActivationKey $ Ascii.encodeBase64Url bs

invalidUser :: ActivationError
invalidUser = InvalidActivationCodeWrongUser -- "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCodeWrongCode -- "Invalid activation code"

codeSelect :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
codeSelect = "SELECT user, code FROM activation_keys WHERE key = ?"
