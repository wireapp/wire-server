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
  ( claimKey,
    keyAvailable,
    lookupKey,
    deleteKey,
    deleteKeyForUser,
  )
where

import Brig.Data.User qualified as User
import Brig.Email
import Cassandra
import Data.Id
import Imports

-- | Claim an 'EmailKey' for a user.
claimKey ::
  MonadClient m =>
  -- | The key to claim.
  EmailKey ->
  -- | The user claiming the key.
  UserId ->
  m Bool
claimKey k u = do
  free <- keyAvailable k (Just u)
  when free (insertKey u k)
  pure free

-- | Check whether an 'EmailKey' is available.
-- A key is available if it is not already actived for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailable ::
  MonadClient m =>
  -- | The key to check.
  EmailKey ->
  -- | The user looking to claim the key, if any.
  Maybe UserId ->
  m Bool
keyAvailable k u = do
  o <- lookupKey k
  case (o, u) of
    (Nothing, _) -> pure True
    (Just x, Just y) | x == y -> pure True
    (Just x, _) -> not <$> User.isActivated x

lookupKey :: MonadClient m => EmailKey -> m (Maybe UserId)
lookupKey k =
  fmap runIdentity
    <$> retry x1 (query1 keySelect (params LocalQuorum (Identity $ emailKeyUniq k)))

insertKey :: MonadClient m => UserId -> EmailKey -> m ()
insertKey u k = do
  retry x5 $ write keyInsert (params LocalQuorum (emailKeyUniq k, u))

deleteKey :: MonadClient m => EmailKey -> m ()
deleteKey k = do
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ emailKeyUniq k))

-- | Delete `EmailKey` for `UserId`
--
-- This function ensures that keys of other users aren't accidentally deleted.
-- E.g. the email address or phone number of a partially deleted user could
-- already belong to a new user. To not interrupt deletion flows (that may be
-- executed several times due to cassandra not supporting transactions)
-- `deleteKeyForUser` does not fail for missing keys or keys that belong to
-- another user: It always returns `()` as result.
deleteKeyForUser :: MonadClient m => UserId -> EmailKey -> m ()
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
