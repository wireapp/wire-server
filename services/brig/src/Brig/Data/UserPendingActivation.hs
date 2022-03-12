{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Brig.Data.UserPendingActivation
  ( usersPendingActivationAdd,
    usersPendingActivationList,
    usersPendingActivationRemove,
    usersPendingActivationRemoveMultiple,
    UserPendingActivation (..),
  )
where

import Cassandra
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

usersPendingActivationAdd :: MonadClient m => UserPendingActivation -> m ()
usersPendingActivationAdd (UserPendingActivation uid expiresAt) = do
  retry x5 . write insertExpiration . params LocalQuorum $ (uid, expiresAt)
  where
    insertExpiration :: PrepQuery W (UserId, UTCTime) ()
    insertExpiration = "INSERT INTO users_pending_activation (user, expires_at) VALUES (?, ?)"

usersPendingActivationList :: MonadClient m => m (Page UserPendingActivation)
usersPendingActivationList = do
  uncurry UserPendingActivation <$$> retry x1 (paginate selectExpired (params LocalQuorum ()))
  where
    selectExpired :: PrepQuery R () (UserId, UTCTime)
    selectExpired =
      "SELECT user, expires_at FROM users_pending_activation"

usersPendingActivationRemove :: MonadClient m => UserId -> m ()
usersPendingActivationRemove uid = usersPendingActivationRemoveMultiple [uid]

usersPendingActivationRemoveMultiple :: MonadClient m => [UserId] -> m ()
usersPendingActivationRemoveMultiple uids =
  retry x5 . write deleteExpired . params LocalQuorum $ Identity uids
  where
    deleteExpired :: PrepQuery W (Identity [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE user IN ?"
