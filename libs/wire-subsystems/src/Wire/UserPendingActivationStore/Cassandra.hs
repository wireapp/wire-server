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
    selectAllUsersPendingActivation,
  )
where

import Cassandra
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports
import Polysemy
import Polysemy.Internal.Tactics
import Wire.UserPendingActivationStore

userPendingActivationStoreToCassandra ::
  forall r.
  (Member (Embed Client) r) =>
  InterpreterFor UserPendingActivationStore r
userPendingActivationStoreToCassandra =
  interpretH $
    liftT . embed @Client . \case
      Add upa -> usersPendingActivationAdd upa
      List -> usersPendingActivationList
      RemoveMultiple uids -> usersPendingActivationRemoveMultiple uids

usersPendingActivationAdd :: (MonadClient m) => UserPendingActivation -> m ()
usersPendingActivationAdd (UserPendingActivation uid expiresAt) = do
  retry x5 . write insertExpiration . params LocalQuorum $ (uid, expiresAt)
  where
    insertExpiration :: PrepQuery W (UserId, UTCTime) ()
    insertExpiration = "INSERT INTO users_pending_activation (user, expires_at) VALUES (?, ?)"

selectAllUsersPendingActivation :: PrepQuery R () (UserId, UTCTime)
selectAllUsersPendingActivation =
  "SELECT user, expires_at FROM users_pending_activation"

usersPendingActivationList :: (MonadClient m) => m [UserPendingActivation]
usersPendingActivationList =
  runConduit $
    paginateC selectAllUsersPendingActivation (paramsP LocalQuorum () 100) x1
      .| C.concat
      .| C.map (uncurry UserPendingActivation)
      .| C.consume

usersPendingActivationRemoveMultiple :: (MonadClient m) => [UserId] -> m ()
usersPendingActivationRemoveMultiple uids =
  retry x5 . write deleteExpired . params LocalQuorum $ Identity uids
  where
    deleteExpired :: PrepQuery W (Identity [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE user IN ?"
