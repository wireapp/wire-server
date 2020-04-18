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

module Gundeck.Notification
  ( paginate,
    PaginateResult (..),
    getById,
    getLast,
  )
where

import Control.Monad.Catch (throwM)
import Data.Id
import Data.Misc (Milliseconds (..))
import Data.Range
import Data.Time.Clock.POSIX
import Gundeck.API.Error
import Gundeck.Monad
import qualified Gundeck.Notification.Data as Data
import Gundeck.Types.Notification
import Imports hiding (getLast)

data PaginateResult
  = PaginateResult
      { paginateResultGap :: Bool,
        paginateResultPage :: QueuedNotificationList
      }

paginate :: UserId -> Maybe NotificationId -> Maybe ClientId -> Range 100 10000 Int32 -> Gundeck PaginateResult
paginate uid since clt size = do
  time <- posixTime
  rs <- Data.fetch uid clt since size
  pure $ PaginateResult (Data.resultGap rs) (resultList time rs)
  where
    resultList time rs =
      queuedNotificationList
        (toList (Data.resultSeq rs))
        (Data.resultHasMore rs)
        (Just (millisToUTC time))
    millisToUTC = posixSecondsToUTCTime . fromIntegral . (`div` 1000) . ms

getById :: UserId -> NotificationId -> Maybe ClientId -> Gundeck QueuedNotification
getById uid nid clt = do
  mn <- Data.fetchId uid nid clt
  maybe (throwM notificationNotFound) return mn

getLast :: UserId -> Maybe ClientId -> Gundeck QueuedNotification
getLast uid clt = do
  mn <- Data.fetchLast uid clt
  maybe (throwM notificationNotFound) return mn
