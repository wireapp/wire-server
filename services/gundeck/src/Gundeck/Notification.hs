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
