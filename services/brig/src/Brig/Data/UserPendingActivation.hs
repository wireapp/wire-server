{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Brig.Data.UserPendingActivation
  ( usersPendingActivationAdd,
    usersPendingActivationList,
    usersPendingActivationRemove,
    usersPendingActivationRemoveMultiple,
    UserPendingActivation (..),
  )
where

import Brig.App (AppIO)
import Cassandra
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

usersPendingActivationAdd :: UserPendingActivation -> AppIO ()
usersPendingActivationAdd (UserPendingActivation uid expiresAt) = do
  retry x5 . write insertExpiration . params Quorum $ (uid, expiresAt)
  where
    insertExpiration :: PrepQuery W (UserId, UTCTime) ()
    insertExpiration = "INSERT INTO users_pending_activation (user, expires_at) VALUES (?, ?)"

usersPendingActivationList :: AppIO (Page UserPendingActivation)
usersPendingActivationList = do
  uncurry UserPendingActivation <$$> retry x1 (paginate selectExpired (params Quorum ()))
  where
    selectExpired :: PrepQuery R () (UserId, UTCTime)
    selectExpired =
      "SELECT user, expires_at FROM users_pending_activation"

usersPendingActivationRemove :: UserId -> AppIO ()
usersPendingActivationRemove uid = usersPendingActivationRemoveMultiple [uid]

usersPendingActivationRemoveMultiple :: [UserId] -> AppIO ()
usersPendingActivationRemoveMultiple uids =
  retry x5 . write deleteExpired . params Quorum $ (Identity uids)
  where
    deleteExpired :: PrepQuery W (Identity [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE user IN ?"
