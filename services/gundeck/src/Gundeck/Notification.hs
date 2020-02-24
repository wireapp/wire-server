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
import Data.Predicate
import Data.Range
import Data.Time.Clock.POSIX
import Gundeck.API.Error
import Gundeck.Monad
import qualified Gundeck.Notification.Data as Data
import Gundeck.Types.Notification
import Gundeck.Util
import Imports hiding (getLast)
import Network.Wai (Response)
import Network.Wai.Utilities

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

getById :: JSON ::: UserId ::: NotificationId ::: Maybe ClientId -> Gundeck Response
getById (_ ::: uid ::: nid ::: clt) = do
  mn <- Data.fetchId uid nid clt
  case mn of
    Nothing -> throwM notificationNotFound
    Just n -> return $ json n

getLast :: JSON ::: UserId ::: Maybe ClientId -> Gundeck Response
getLast (_ ::: uid ::: clt) = do
  n <- Data.fetchLast uid clt
  maybe (throwM notificationNotFound) (return . json) n
