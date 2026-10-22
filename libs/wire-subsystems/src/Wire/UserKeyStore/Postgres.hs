-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserKeyStore.Postgres
  ( interpretUserKeyStoreToPostgres,
    upsertUserKey,
  )
where

import Data.Id (UserId)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.Postgres
import Wire.UserKeyStore
import Wire.UserStore

interpretUserKeyStoreToPostgres ::
  (PGConstraints r, Member UserStore r) =>
  InterpreterFor UserKeyStore r
interpretUserKeyStoreToPostgres = interpret $ \case
  LookupKey key -> lookupKeyImpl key
  InsertKey uid key -> upsertUserKey (emailKeyUniq key) uid
  DeleteKey key -> deleteKeyImpl key
  DeleteKeyForUser uid key -> deleteKeyForUserImpl uid key
  ClaimKey key uid -> claimKeyImpl key uid
  KeyAvailable key uid -> keyAvailableImpl key uid

-- | Claim an 'EmailKey' for a user.
--
-- Faithful port of the Cassandra implementation: a read-then-write sequence
-- (no guarded insert). 'ClaimKey' is only available if the key is unclaimed or
-- already owned by the claiming user's (deactivated) account.
claimKeyImpl :: (PGConstraints r, Member UserStore r) => EmailKey -> UserId -> Sem r Bool
claimKeyImpl k u = do
  free <- keyAvailableImpl k (Just u)
  when free (upsertUserKey (emailKeyUniq k) u)
  pure free

-- | Check whether an 'EmailKey' is available.
-- A key is available if it is not already activated for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailableImpl :: (PGConstraints r, Member UserStore r) => EmailKey -> Maybe UserId -> Sem r Bool
keyAvailableImpl k u = do
  o <- lookupKeyImpl k
  case (o, u) of
    (Nothing, _) -> pure True
    (Just x, Just y) | x == y -> pure True
    (Just x, _) -> not <$> isActivated x

lookupKeyImpl :: (PGConstraints r) => EmailKey -> Sem r (Maybe UserId)
lookupKeyImpl k =
  runStatement (emailKeyUniq k) lookupStatement
  where
    lookupStatement :: Hasql.Statement Text (Maybe UserId)
    lookupStatement =
      dimapPG
        [maybeStatement|SELECT (user_id :: uuid)
                        FROM user_keys
                        WHERE key = $1 :: text|]

upsertUserKey :: (PGConstraints r) => Text -> UserId -> Sem r ()
upsertUserKey key uid =
  runStatement (key, uid) upsertStatement
  where
    upsertStatement :: Hasql.Statement (Text, UserId) ()
    upsertStatement =
      lmapPG
        [resultlessStatement|INSERT INTO user_keys (key, user_id)
                             VALUES ($1 :: text, $2 :: uuid)
                             ON CONFLICT (key) DO UPDATE SET user_id = EXCLUDED.user_id|]

deleteKeyImpl :: (PGConstraints r) => EmailKey -> Sem r ()
deleteKeyImpl k =
  runStatement (emailKeyUniq k) deleteStatement
  where
    deleteStatement :: Hasql.Statement Text ()
    deleteStatement =
      [resultlessStatement|DELETE FROM user_keys
                           WHERE key = $1 :: text|]

-- | Delete an 'EmailKey' for a 'UserId'.
--
-- Unlike the Cassandra implementation (read-then-delete), this is a single
-- guarded delete, so keys of other users cannot be deleted accidentally.
deleteKeyForUserImpl :: (PGConstraints r) => UserId -> EmailKey -> Sem r ()
deleteKeyForUserImpl uid k =
  runStatement (uid, emailKeyUniq k) deleteForUserStatement
  where
    deleteForUserStatement :: Hasql.Statement (UserId, Text) ()
    deleteForUserStatement =
      lmapPG
        [resultlessStatement|DELETE FROM user_keys
                             WHERE user_id = $1 :: uuid
                               AND key = $2 :: text|]
