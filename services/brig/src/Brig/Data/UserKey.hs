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
  ( userEmailKey,
    userPhoneKey,
    forEmailKey,
    forPhoneKey,
    foldKey,
    keyText,
    keyTextOriginal,
    claimKey,
    keyAvailable,
    getKey,
    deleteKey,
    deleteKeyForUser,
    lookupPhoneHashes,
  )
where

import Brig.Data.Instances ()
import qualified Brig.Data.User as User
import Brig.Effects.UserKeyStore
import Brig.Effects.UserQuery (UserQuery)
import Brig.Email
import Brig.Phone
import Brig.Types.Common
import Cassandra
import qualified Data.ByteString as B
import Data.Id
import qualified Data.Multihash.Digest as MH
import Imports
import OpenSSL.EVP.Digest (Digest)
import Polysemy
import Wire.API.User (fromEmail)

userEmailKey :: Email -> UserKey
userEmailKey = UserEmailKey . mkEmailKey

userPhoneKey :: Phone -> UserKey
userPhoneKey = UserPhoneKey . mkPhoneKey

forEmailKey :: Applicative f => UserKey -> (Email -> f a) -> f (Maybe a)
forEmailKey k f = foldKey (fmap Just . f) (const (pure Nothing)) k

forPhoneKey :: Applicative f => UserKey -> (Phone -> f a) -> f (Maybe a)
forPhoneKey k f = foldKey (const (pure Nothing)) (fmap Just . f) k

-- | Get the original text of a 'UserKey', i.e. the original phone number
-- or email address.
keyTextOriginal :: UserKey -> Text
keyTextOriginal (UserEmailKey k) = fromEmail (emailKeyOrig k)
keyTextOriginal (UserPhoneKey k) = fromPhone (phoneKeyOrig k)

-- | Claim a 'UserKey' for a user.
claimKey ::
  Members '[UserKeyStore, UserQuery] r =>
  -- | The SHA256 digest
  Digest ->
  -- | The key to claim.
  UserKey ->
  -- | The user claiming the key.
  UserId ->
  Sem r Bool
claimKey d k u = do
  free <- keyAvailable k (Just u)
  when free (insertKey d u k)
  pure free

-- | Check whether a 'UserKey' is available.
-- A key is available if it is not already actived for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailable ::
  Members '[UserKeyStore, UserQuery] r =>
  -- | The key to check.
  UserKey ->
  -- | The user looking to claim the key, if any.
  Maybe UserId ->
  Sem r Bool
keyAvailable k u = do
  o <- getKey k
  case (o, u) of
    (Nothing, _) -> pure True
    (Just x, Just y) | x == y -> pure True
    (Just x, _) -> not <$> User.isActivated x

-- | Delete `UserKey` for `UserId`
--
-- This function ensures that keys of other users aren't accidentally deleted.
-- E.g. the email address or phone number of a partially deleted user could
-- already belong to a new user. To not interrupt deletion flows (that may be
-- executed several times due to cassandra not supporting transactions)
-- `deleteKeyForUser` does not fail for missing keys or keys that belong to
-- another user: It always returns `()` as result.
deleteKeyForUser ::
  Member UserKeyStore r =>
  Digest ->
  UserId ->
  UserKey ->
  Sem r ()
deleteKeyForUser d uid k = do
  mbKeyUid <- getKey k
  case mbKeyUid of
    Just keyUid | keyUid == uid -> deleteKey d k
    _ -> pure ()

lookupPhoneHashes :: MonadClient m => [ByteString] -> m [(ByteString, UserId)]
lookupPhoneHashes hp =
  mapMaybe mk <$> retry x1 (query selectHashed (params One (Identity hashed)))
  where
    hashed = fmap (\h -> UserKeyHash $ MH.MultihashDigest MH.SHA256 (B.length h) h) hp
    mk (UserKeyHash d, UKHashPhone, u) = Just (MH.digest d, u)
    mk (_, _, _) = Nothing

--------------------------------------------------------------------------------
-- Queries

selectHashed :: PrepQuery R (Identity [UserKeyHash]) (UserKeyHash, UKHashType, UserId)
selectHashed = "SELECT key, key_type, user FROM user_keys_hash WHERE key IN ?"
