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

module Brig.Data.PendingActivation
  ( trackExpiration,
    searchTrackedExpirations,
    removeTrackedExpiration,
    PendingActivationExpiration (..),
  )
where

import Brig.App (AppIO)
import Cassandra
import Data.Id (TeamId, UserId)
import Data.Misc (ModJulianDay (..))
import Data.Time (UTCTime (utctDay))
import Data.Time.Calendar (Day)
import Data.Tuple.Extra (uncurry3)
import Imports

data PendingActivationExpiration
  = PendingActivationExpiration
      !UserId
      !UTCTime
      !TeamId
  deriving stock (Eq)

-- | Same as clustering order
instance Ord PendingActivationExpiration where
  (PendingActivationExpiration uid t _) <= (PendingActivationExpiration uid2 t2 _) =
    (t, uid) <= (t2, uid2)

-- | Note: Call this function only after an invitation for the user has been created
trackExpiration :: PendingActivationExpiration -> AppIO ()
trackExpiration (PendingActivationExpiration uid expiresAt tid) = do
  retry x5 . write insertExpiration . params Quorum $ (ModJulianDay (utctDay expiresAt), expiresAt, uid, tid)
  where
    insertExpiration :: PrepQuery W (ModJulianDay, UTCTime, UserId, TeamId) ()
    insertExpiration = "INSERT INTO users_pending_activation (expires_at_day, expires_at, user, team) VALUES (?, ?, ?, ?)"

searchTrackedExpirations :: MonadClient f => Day -> f (Page PendingActivationExpiration)
searchTrackedExpirations dayExpired = do
  uncurry3 PendingActivationExpiration
    <$$> retry x1 (paginate selectExpired (params Quorum (Identity (ModJulianDay dayExpired))))
  where
    selectExpired :: PrepQuery R (Identity ModJulianDay) (UserId, UTCTime, TeamId)
    selectExpired =
      "SELECT user, expires_at, team FROM users_pending_activation \
      \WHERE expires_at_day = ?"

removeTrackedExpiration :: Day -> [UserId] -> AppIO ()
removeTrackedExpiration day uids =
  retry x5 . write deleteExpired . params Quorum $ (ModJulianDay day, uids)
  where
    deleteExpired :: PrepQuery W (ModJulianDay, [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE expires_at_day = ? and user in ?"
