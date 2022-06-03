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

-- | Natural, addressable external identifiers of users.
module Brig.Data.UserKey
  ( UserKey,
    userEmailKey,
    userPhoneKey,
    forEmailKey,
    forPhoneKey,
    foldKey,
    keyText,
    keyTextOriginal,
    claimKey,
    keyAvailable,
    lookupKey,
    deleteKey,
    lookupPhoneHashes,
  )
where

import Brig.Data.Instances ()
import qualified Brig.Data.User as User
import Brig.Email
import Brig.Phone
import Brig.Types
import Cassandra
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.Id
import qualified Data.Multihash.Digest as MH
import qualified Data.Text.Encoding as T
import Imports
import OpenSSL.EVP.Digest (Digest, digestBS)

-- | A natural identifier (i.e. unique key) of a user.
data UserKey
  = UserEmailKey !EmailKey
  | UserPhoneKey !PhoneKey

instance Eq UserKey where
  (UserEmailKey k) == (UserEmailKey k') = k == k'
  (UserPhoneKey k) == (UserPhoneKey k') = k == k'
  _ == _ = False

data UKHashType
  = UKHashPhone
  | UKHashEmail
  deriving (Eq)

instance Cql UKHashType where
  ctype = Tagged IntColumn

  fromCql (CqlInt i) = case i of
    0 -> pure UKHashPhone
    1 -> pure UKHashEmail
    n -> Left $ "unexpected hashtype: " ++ show n
  fromCql _ = Left "userkeyhashtype: int expected"

  toCql UKHashPhone = CqlInt 0
  toCql UKHashEmail = CqlInt 1

newtype UserKeyHash = UserKeyHash MH.MultihashDigest

instance Cql UserKeyHash where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob lbs) = case MH.decode (toStrict lbs) of
    Left e -> Left ("userkeyhash: " ++ e)
    Right h -> pure $ UserKeyHash h
  fromCql _ = Left "userkeyhash: expected blob"

  toCql (UserKeyHash d) = CqlBlob $ MH.encode (MH.algorithm d) (MH.digest d)

userEmailKey :: Email -> UserKey
userEmailKey = UserEmailKey . mkEmailKey

userPhoneKey :: Phone -> UserKey
userPhoneKey = UserPhoneKey . mkPhoneKey

foldKey :: (Email -> a) -> (Phone -> a) -> UserKey -> a
foldKey f g k = case k of
  UserEmailKey ek -> f (emailKeyOrig ek)
  UserPhoneKey pk -> g (phoneKeyOrig pk)

forEmailKey :: Applicative f => UserKey -> (Email -> f a) -> f (Maybe a)
forEmailKey k f = foldKey (fmap Just . f) (const (pure Nothing)) k

forPhoneKey :: Applicative f => UserKey -> (Phone -> f a) -> f (Maybe a)
forPhoneKey k f = foldKey (const (pure Nothing)) (fmap Just . f) k

-- | Get the normalised text of a 'UserKey'.
keyText :: UserKey -> Text
keyText (UserEmailKey k) = emailKeyUniq k
keyText (UserPhoneKey k) = phoneKeyUniq k

-- | Get the original text of a 'UserKey', i.e. the original phone number
-- or email address.
keyTextOriginal :: UserKey -> Text
keyTextOriginal (UserEmailKey k) = fromEmail (emailKeyOrig k)
keyTextOriginal (UserPhoneKey k) = fromPhone (phoneKeyOrig k)

-- | Claim a 'UserKey' for a user.
claimKey ::
  MonadClient m =>
  -- | The SHA256 digest
  Digest ->
  -- | The key to claim.
  UserKey ->
  -- | The user claiming the key.
  UserId ->
  m Bool
claimKey d k u = do
  free <- keyAvailable k (Just u)
  when free (insertKey d u k)
  pure free

-- | Check whether a 'UserKey' is available.
-- A key is available if it is not already actived for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailable ::
  MonadClient m =>
  -- | The key to check.
  UserKey ->
  -- | The user looking to claim the key, if any.
  Maybe UserId ->
  m Bool
keyAvailable k u = do
  o <- lookupKey k
  case (o, u) of
    (Nothing, _) -> pure True
    (Just x, Just y) | x == y -> pure True
    (Just x, _) -> not <$> User.isActivated x

lookupKey :: MonadClient m => UserKey -> m (Maybe UserId)
lookupKey k =
  fmap runIdentity
    <$> retry x1 (query1 keySelect (params LocalQuorum (Identity $ keyText k)))

insertKey :: MonadClient m => Digest -> UserId -> UserKey -> m ()
insertKey d u k = do
  let hk = hashKey d k
      kt = foldKey (\(_ :: Email) -> UKHashEmail) (\(_ :: Phone) -> UKHashPhone) k
  retry x5 $ write insertHashed (params LocalQuorum (hk, kt, u))
  retry x5 $ write keyInsert (params LocalQuorum (keyText k, u))

deleteKey :: MonadClient m => Digest -> UserKey -> m ()
deleteKey d k = do
  let hk = hashKey d k
  retry x5 $ write deleteHashed (params LocalQuorum (Identity hk))
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ keyText k))

hashKey :: Digest -> UserKey -> UserKeyHash
hashKey d uk =
  let d' = digestBS d $ T.encodeUtf8 (keyText uk)
   in UserKeyHash $ MH.MultihashDigest MH.SHA256 (B.length d') d'

lookupPhoneHashes :: MonadClient m => [ByteString] -> m [(ByteString, UserId)]
lookupPhoneHashes hp =
  mapMaybe mk <$> retry x1 (query selectHashed (params One (Identity hashed)))
  where
    hashed = fmap (\h -> UserKeyHash $ MH.MultihashDigest MH.SHA256 (B.length h) h) hp
    mk (UserKeyHash d, UKHashPhone, u) = Just (MH.digest d, u)
    mk (_, _, _) = Nothing

--------------------------------------------------------------------------------
-- Queries

keyInsert :: PrepQuery W (Text, UserId) ()
keyInsert = "INSERT INTO user_keys (key, user) VALUES (?, ?)"

keySelect :: PrepQuery R (Identity Text) (Identity UserId)
keySelect = "SELECT user FROM user_keys WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM user_keys WHERE key = ?"

insertHashed :: PrepQuery W (UserKeyHash, UKHashType, UserId) ()
insertHashed = "INSERT INTO user_keys_hash(key, key_type, user) VALUES (?, ?, ?)"

deleteHashed :: PrepQuery W (Identity UserKeyHash) ()
deleteHashed = "DELETE FROM user_keys_hash WHERE key = ?"

selectHashed :: PrepQuery R (Identity [UserKeyHash]) (UserKeyHash, UKHashType, UserId)
selectHashed = "SELECT key, key_type, user FROM user_keys_hash WHERE key IN ?"
