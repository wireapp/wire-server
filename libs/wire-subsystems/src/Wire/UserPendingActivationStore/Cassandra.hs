{-# LANGUAGE DeepSubsumption #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserPendingActivationStore.Cassandra
  ( userPendingActivationStoreToCassandra,
  )
where

import Cassandra
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports
import Polysemy
import Wire.UserPendingActivationStore

userPendingActivationStoreToCassandra ::
  (Member (Embed Client) r) =>
  InterpreterFor UserPendingActivationStore r
userPendingActivationStoreToCassandra =
  interpret $
    embed @Client . \case
      Add upa -> usersPendingActivationAdd upa
      ListPage after limit -> usersPendingActivationListPage after limit
      RemoveMultiple uids -> usersPendingActivationRemoveMultiple uids

usersPendingActivationAdd :: (MonadClient m) => UserPendingActivation -> m ()
usersPendingActivationAdd (UserPendingActivation uid expiresAt) = do
  retry x5 . write insertExpiration . params LocalQuorum $ (uid, expiresAt)
  where
    insertExpiration :: PrepQuery W (UserId, UTCTime) ()
    insertExpiration = "INSERT INTO users_pending_activation (user, expires_at) VALUES (?, ?)"

-- | Token-range scan: rows come in partition token order, so the next page
-- starts after the token of the last user seen.
usersPendingActivationListPage :: (MonadClient m) => Maybe UserId -> Int32 -> m ([UserPendingActivation], Bool)
usersPendingActivationListPage after limit = do
  rows <- retry x1 $ case after of
    Nothing -> query selectFirst (params LocalQuorum (Identity (limit + 1)))
    Just uid -> query selectAfter (params LocalQuorum (uid, limit + 1))
  pure
    ( uncurry UserPendingActivation <$> take (fromIntegral limit) rows,
      length rows > fromIntegral limit
    )
  where
    selectFirst :: PrepQuery R (Identity Int32) (UserId, UTCTime)
    selectFirst =
      "SELECT user, expires_at FROM users_pending_activation LIMIT ?"
    selectAfter :: PrepQuery R (UserId, Int32) (UserId, UTCTime)
    selectAfter =
      "SELECT user, expires_at FROM users_pending_activation WHERE token(user) > token(?) LIMIT ?"

usersPendingActivationRemoveMultiple :: (MonadClient m) => [UserId] -> m ()
usersPendingActivationRemoveMultiple uids =
  retry x5 . write deleteExpired . params LocalQuorum $ Identity uids
  where
    deleteExpired :: PrepQuery W (Identity [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE user IN ?"
