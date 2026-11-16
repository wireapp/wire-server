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
  InsertKey uid key -> insertKeyImpl key uid
  DeleteKey key -> deleteKeyImpl key
  DeleteKeyForUser uid key -> deleteKeyForUserImpl uid key
  ClaimKey key uid -> claimKeyImpl key uid
  KeyAvailable key uid -> keyAvailableImpl key uid

-- | Claim an 'EmailKey' for a user.
--
-- The key is claimable if it is unclaimed, already owned by the claiming user,
-- or owned by a user that is not activated (see 'keyAvailableImpl'). Whether
-- the owner is activated lives in the 'UserStore', so it cannot be checked in
-- the same SQL statement. Instead, the write is a compare-and-swap on the owner
-- that was observed when checking availability: it only succeeds if the key is
-- still unclaimed, still owned by that same (non-activated) user, or already
-- owned by the claiming user. If a concurrent claim won in between, the
-- statement affects no row and the claim fails, instead of silently taking
-- the key over.
claimKeyImpl :: (PGConstraints r, Member UserStore r) => EmailKey -> UserId -> Sem r Bool
claimKeyImpl k u = do
  owner <- lookupKeyImpl k
  free <- isFree owner (Just u)
  if free
    then runStatement (emailKeyUniq k, u, owner) claimStatement
    else pure False
  where
    claimStatement :: Hasql.Statement (Text, UserId, Maybe UserId) Bool
    claimStatement =
      isJust
        <$> lmapPG
          [maybeStatement|INSERT INTO user_keys (key, user_id)
                        VALUES ($1 :: text, $2 :: uuid)
                        ON CONFLICT (key) DO UPDATE SET user_id = EXCLUDED.user_id
                        WHERE user_keys.user_id = EXCLUDED.user_id
                           OR user_keys.user_id = $3 :: uuid?
                        RETURNING (user_id :: uuid)|]

-- | Check whether an 'EmailKey' is available.
-- A key is available if it is not already activated for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailableImpl :: (PGConstraints r, Member UserStore r) => EmailKey -> Maybe UserId -> Sem r Bool
keyAvailableImpl k u = do
  o <- lookupKeyImpl k
  isFree o u

isFree :: (Member UserStore r) => Maybe UserId -> Maybe UserId -> Sem r Bool
isFree o u =
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

-- | Insert (or overwrite) an 'EmailKey' for a user, like the Cassandra
-- implementation does.
insertKeyImpl :: (PGConstraints r) => EmailKey -> UserId -> Sem r ()
insertKeyImpl k uid =
  runStatement (emailKeyUniq k, uid) upsertStatement
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
