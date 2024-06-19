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
    deleteKeyForUser,
  )
where

import Brig.Data.User qualified as User
import Brig.Email
import Brig.Phone
import Cassandra
import Data.Id
import Imports
import Wire.API.User (fromEmail)

-- | A natural identifier (i.e. unique key) of a user.
data UserKey
  = UserEmailKey !EmailKey
  | UserPhoneKey !PhoneKey
  deriving stock (Eq, Show)

userEmailKey :: Email -> UserKey
userEmailKey = UserEmailKey . mkEmailKey

userPhoneKey :: Phone -> UserKey
userPhoneKey = UserPhoneKey . mkPhoneKey

foldKey :: (Email -> a) -> (Phone -> a) -> UserKey -> a
foldKey f g k = case k of
  UserEmailKey ek -> f (emailKeyOrig ek)
  UserPhoneKey pk -> g (phoneKeyOrig pk)

forEmailKey :: (Applicative f) => UserKey -> (Email -> f a) -> f (Maybe a)
forEmailKey k f = foldKey (fmap Just . f) (const (pure Nothing)) k

forPhoneKey :: (Applicative f) => UserKey -> (Phone -> f a) -> f (Maybe a)
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
  (MonadClient m) =>
  -- | The key to claim.
  UserKey ->
  -- | The user claiming the key.
  UserId ->
  m Bool
claimKey k u = do
  free <- keyAvailable k (Just u)
  when free (insertKey u k)
  pure free

-- | Check whether a 'UserKey' is available.
-- A key is available if it is not already actived for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailable ::
  (MonadClient m) =>
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

lookupKey :: (MonadClient m) => UserKey -> m (Maybe UserId)
lookupKey k =
  fmap runIdentity
    <$> retry x1 (query1 keySelect (params LocalQuorum (Identity $ keyText k)))

insertKey :: (MonadClient m) => UserId -> UserKey -> m ()
insertKey u k = do
  retry x5 $ write keyInsert (params LocalQuorum (keyText k, u))

deleteKey :: (MonadClient m) => UserKey -> m ()
deleteKey k = do
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ keyText k))

-- | Delete `UserKey` for `UserId`
--
-- This function ensures that keys of other users aren't accidentally deleted.
-- E.g. the email address or phone number of a partially deleted user could
-- already belong to a new user. To not interrupt deletion flows (that may be
-- executed several times due to cassandra not supporting transactions)
-- `deleteKeyForUser` does not fail for missing keys or keys that belong to
-- another user: It always returns `()` as result.
deleteKeyForUser :: (MonadClient m) => UserId -> UserKey -> m ()
deleteKeyForUser uid k = do
  mbKeyUid <- lookupKey k
  case mbKeyUid of
    Just keyUid | keyUid == uid -> deleteKey k
    _ -> pure ()

--------------------------------------------------------------------------------
-- Queries

keyInsert :: PrepQuery W (Text, UserId) ()
keyInsert = "INSERT INTO user_keys (key, user) VALUES (?, ?)"

keySelect :: PrepQuery R (Identity Text) (Identity UserId)
keySelect = "SELECT user FROM user_keys WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM user_keys WHERE key = ?"
