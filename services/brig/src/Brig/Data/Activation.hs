{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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
import Brig.Data.PasswordReset
import Brig.Data.User
import Brig.Data.UserKey
import Brig.Options
import Brig.Sem.ActivationQueryStore
import Brig.Types
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

-- | Max. number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3

-- docs/reference/user/activation.md {#RefActivationSubmit}

-- TODO(md): Drop the Member (Embed m) r constraint once we have effects for
-- lookupAccount and claim
activateKey ::
  forall m r.
  ( MonadClient m,
    MonadReader Env m,
    Member ActivationQueryStore r,
    Member (Embed m) r
  ) =>
  ActivationKey ->
  ActivationCode ->
  Maybe UserId ->
  ExceptT ActivationError (Sem r) (Maybe ActivationEvent)
activateKey k c u = verifyCode k c >>= pickUser >>= activate
  where
    pickUser (uk, u') = maybe (throwE invalidUser) (return . (uk,)) (u <|> u')
    activate (key, uid) = do
      a <- lift (embed @m $ lookupAccount uid) >>= maybe (throwE invalidUser) return
      unless (accountStatus a == Active) $ -- this is never 'PendingActivation' in the flow this function is used in.
        throwE invalidCode
      case userIdentity (accountUser a) of
        Nothing -> do
          mapExceptT (embed @m) $ claim key uid
          let ident = foldKey EmailIdentity PhoneIdentity key
          lift . embed @m $ activateUser uid ident
          let a' = a {accountUser = (accountUser a) {userIdentity = Just ident}}
          return . Just $ AccountActivated a'
        Just _ -> do
          let usr = accountUser a
              (profileNeedsUpdate, oldKey) =
                foldKey
                  (\(e :: Email) -> (Just e /= userEmail usr,) . fmap userEmailKey . userEmail)
                  (\(p :: Phone) -> (Just p /= userPhone usr,) . fmap userPhoneKey . userPhone)
                  key
                  usr
           in mapExceptT (embed @m) $ handleExistingIdentity uid profileNeedsUpdate oldKey key
    handleExistingIdentity uid profileNeedsUpdate oldKey key
      | oldKey == Just key && not profileNeedsUpdate = return Nothing
      -- activating existing key and exactly same profile
      -- (can happen when a user clicks on activation links more than once)
      | oldKey == Just key && profileNeedsUpdate = do
        lift $ foldKey (updateEmail uid) (updatePhone uid) key
        return . Just $ foldKey (EmailActivated uid) (PhoneActivated uid) key
      -- if the key is the same, we only want to update our profile
      | otherwise = do
        mkPasswordResetKey uid >>= lift . deletePasswordResetCode
        claim key uid
        lift $ foldKey (updateEmail uid) (updatePhone uid) key
        for_ oldKey $ lift . deleteKey
        return . Just $ foldKey (EmailActivated uid) (PhoneActivated uid) key
    claim key uid = do
      ok <- lift $ claimKey key uid
      unless ok $
        throwE . UserKeyExists . LT.fromStrict $
          foldKey fromEmail fromPhone key

-- TODO(md): Member (Embed m) r is here only because of mkActivationKey

-- | Create a new pending activation for a given 'UserKey'.
newActivation ::
  forall m r.
  ( MonadIO m,
    MonadClient m,
    Member ActivationQueryStore r,
    Member (Embed m) r
  ) =>
  UserKey ->
  -- | The timeout for the activation code.
  Timeout ->
  -- | The user with whom to associate the activation code.
  Maybe UserId ->
  Sem r Activation
newActivation uk timeout u = do
  (typ, key, code) <-
    embed @m . liftIO $
      foldKey
        (\e -> ("email",fromEmail e,) <$> genCode)
        (\p -> ("phone",fromPhone p,) <$> genCode)
        uk
  insert typ key code
  where
    insert t k c = do
      key <- embed @m $ liftIO $ mkActivationKey uk
      keyInsert key t k c u maxAttempts (round timeout)
      return $ Activation key c
    genCode =
      ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
        <$> randIntegerZeroToNMinusOne 1000000

-- TODO(md): Member (Embed m) r is here only because of mkActivationKey

-- | Lookup an activation code and it's associated owner (if any) for a 'UserKey'.
lookupActivationCode ::
  forall m r.
  ( MonadClient m,
    Member ActivationQueryStore r,
    Member (Embed m) r
  ) =>
  UserKey ->
  Sem r (Maybe (Maybe UserId, ActivationCode))
lookupActivationCode k =
  embed @m (liftIO (mkActivationKey k))
    >>= codeSelect

-- | Verify an activation code.
verifyCode ::
  (Member ActivationQueryStore r) =>
  ActivationKey ->
  ActivationCode ->
  ExceptT ActivationError (Sem r) (UserKey, Maybe UserId)
verifyCode key code = do
  s <- lift . keySelect $ key
  case s of
    Just (ttl, Ascii t, k, c, u, r) ->
      if
          | c == code -> mkScope t k u
          | r >= 1 -> lift (keyInsert key t k c u (r - 1) ttl) >> throwE invalidCode
          | otherwise -> revoke >> throwE invalidCode
    Nothing -> throwE invalidCode
  where
    mkScope "email" k u = case parseEmail k of
      Just e -> return (userEmailKey e, u)
      Nothing -> throwE invalidCode
    mkScope "phone" k u = case parsePhone k of
      Just p -> return (userPhoneKey p, u)
      Nothing -> throwE invalidCode
    mkScope _ _ _ = throwE invalidCode
    revoke = lift $ keyDelete key

mkActivationKey :: UserKey -> IO ActivationKey
mkActivationKey k = do
  d <- liftIO $ getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") return d
  let bs = digestBS d' (T.encodeUtf8 $ keyText k)
  return . ActivationKey $ Ascii.encodeBase64Url bs

invalidUser :: ActivationError
invalidUser = InvalidActivationCodeWrongUser -- "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCodeWrongCode -- "Invalid activation code"
