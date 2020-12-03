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
  ( trackExpiration,
    searchTrackedExpirations,
    removeTrackedExpirations,
    UserPendingActivation (..),
  )
where

import Brig.App (AppIO)
import Cassandra
import Data.Id (TeamId, UserId)
import Data.Misc (ModJulianDay (..))
import Data.Time.Calendar (Day)
import Imports

data UserPendingActivation = UserPendingActivation
  { upaDay :: !Day,
    upaUserId :: !UserId,
    upaTeamId :: !TeamId
  }
  deriving stock (Eq)

-- | Note: Call this function only after an invitation for the user has been created
trackExpiration :: UserPendingActivation -> AppIO ()
trackExpiration (UserPendingActivation expiresAtDay uid tid) = do
  retry x5 . write insertExpiration . params Quorum $ (ModJulianDay expiresAtDay, uid, tid)
  where
    insertExpiration :: PrepQuery W (ModJulianDay, UserId, TeamId) ()
    insertExpiration = "INSERT INTO users_pending_activation (expires_at_day, user, team) VALUES (?, ?, ?)"

searchTrackedExpirations :: MonadClient f => Day -> Int -> f [UserPendingActivation]
searchTrackedExpirations dayExpired pageSize = do
  (\(ModJulianDay d, uid, tid) -> UserPendingActivation d uid tid)
    <$$> retry x1 (query selectExpired (params Quorum (ModJulianDay dayExpired, fromIntegral pageSize)))
  where
    selectExpired :: PrepQuery R (ModJulianDay, Int32) (ModJulianDay, UserId, TeamId)
    selectExpired =
      "SELECT expires_at, user, team FROM users_pending_activation \
      \WHERE expires_at_day = ? LIMIT ?"

removeTrackedExpirations :: Day -> [UserId] -> AppIO ()
removeTrackedExpirations day uids =
  retry x5 . write deleteExpired . params Quorum $ (ModJulianDay day, uids)
  where
    deleteExpired :: PrepQuery W (ModJulianDay, [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE expires_at_day = ? and user in ?"
