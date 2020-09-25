-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    newActivation,
    mkActivationKey,
    lookupActivationCode,
    activateKey,
    verifyCode,
  )
where

import Brig.App (AppIO)
import Brig.Data.PasswordReset
import Brig.Data.User
import Brig.Data.UserKey
import Brig.Options
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
import Text.Printf (printf)

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
  | InvalidActivationCode !LT.Text
  | InvalidActivationEmail !Email !String
  | InvalidActivationPhone !Phone

data ActivationEvent
  = AccountActivated !UserAccount
  | EmailActivated !UserId !Email
  | PhoneActivated !UserId !Phone

-- | Max. number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKey ::
  ActivationKey ->
  ActivationCode ->
  Maybe UserId ->
  ExceptT ActivationError AppIO (Maybe ActivationEvent)
activateKey k c u = verifyCode k c >>= pickUser >>= activate
  where
    pickUser (uk, u') = maybe (throwE invalidUser) (return . (uk,)) (u <|> u')
    activate (key, uid) = do
      a <- lift (lookupAccount uid) >>= maybe (throwE invalidUser) return
      unless (accountStatus a == Active) $
        throwE invalidCode
      case userIdentity (accountUser a) of
        Nothing -> do
          claim key uid
          let ident = foldKey EmailIdentity PhoneIdentity key
          lift $ activateUser uid ident
          let a' = a {accountUser = (accountUser a) {userIdentity = Just ident}}
          return . Just $ AccountActivated a'
        Just _ -> do
          let usr = accountUser a
              (profileNeedsUpdate, oldKey) =
                foldKey
                  (\(e :: Email) -> (Just e /= userEmail usr,) . fmap userEmailKey . userEmail)
                  (\(p :: Phone) -> (Just p /= userPhone usr,) . fmap userPhoneKey . userPhone)
                  key
                  $ usr
           in handleExistingIdentity uid profileNeedsUpdate oldKey key
    handleExistingIdentity uid profileNeedsUpdate oldKey key
      | oldKey == Just key && (not profileNeedsUpdate) = return Nothing
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

-- | Create a new pending activation for a given 'UserKey'.
newActivation ::
  UserKey ->
  -- | The timeout for the activation code.
  Timeout ->
  -- | The user with whom to associate the activation code.
  Maybe UserId ->
  AppIO Activation
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
      retry x5 . write keyInsert $ params Quorum (key, t, k, c, u, maxAttempts, round timeout)
      return $ Activation key c
    genCode =
      ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
        <$> randIntegerZeroToNMinusOne 1000000

-- | Lookup an activation code and it's associated owner (if any) for a 'UserKey'.
lookupActivationCode :: UserKey -> AppIO (Maybe (Maybe UserId, ActivationCode))
lookupActivationCode k =
  liftIO (mkActivationKey k)
    >>= retry x1 . query1 codeSelect . params Quorum . Identity

-- | Verify an activation code.
verifyCode ::
  ActivationKey ->
  ActivationCode ->
  ExceptT ActivationError AppIO (UserKey, Maybe UserId)
verifyCode key code = do
  s <- lift . retry x1 . query1 keySelect $ params Quorum (Identity key)
  case s of
    Just (ttl, Ascii t, k, c, u, r) ->
      if
          | c == code -> mkScope t k u
          | r >= 1 -> countdown (key, t, k, c, u, r -1, ttl) >> throwE invalidCode
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
    countdown = lift . retry x5 . write keyInsert . params Quorum
    revoke = lift $ deleteActivationPair key

mkActivationKey :: UserKey -> IO ActivationKey
mkActivationKey k = do
  d <- liftIO $ getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") return d
  let bs = digestBS d' (T.encodeUtf8 $ keyText k)
  return . ActivationKey $ Ascii.encodeBase64Url bs

deleteActivationPair :: ActivationKey -> AppIO ()
deleteActivationPair = write keyDelete . params Quorum . Identity

invalidUser :: ActivationError
invalidUser = InvalidActivationCode "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCode "Invalid activation code"

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
