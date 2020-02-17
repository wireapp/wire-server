module Gundeck.Notification
  ( paginate,
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
import qualified Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import Gundeck.API.Error
import Gundeck.Monad
import qualified Gundeck.Notification.Data as Data
import Gundeck.Types.Notification
import Gundeck.Util
import Imports hiding (getLast)
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Utilities

paginate :: JSON ::: UserId ::: Maybe ByteString ::: Maybe ClientId ::: Range 100 10000 Int32 -> Gundeck Response
paginate (_ ::: uid ::: Nothing ::: clt ::: size) = do
  t <- posixTime
  pageResponse t <$> Data.fetch uid clt Nothing size
paginate (_ ::: uid ::: Just since ::: clt ::: size) = do
  t <- posixTime
  case parseUUID since of
    Nothing ->
      setStatus status404 . pageResponse t
        <$> Data.fetch uid clt Nothing size
    Just s -> do
      pageResponse t <$> Data.fetch uid clt (Just s) size
  where
    parseUUID = UUID.fromASCIIBytes >=> isV1UUID >=> return . Id
    isV1UUID u = if UUID.version u == 1 then Just u else Nothing

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

pageResponse :: Milliseconds -> Data.ResultPage -> Response
pageResponse t rs
  | Data.resultGap rs = setStatus status404 (json resultList)
  | otherwise = json resultList
  where
    resultList =
      queuedNotificationList
        (toList (Data.resultSeq rs))
        (Data.resultHasMore rs)
        (Just (millisToUTC t))
    millisToUTC = posixSecondsToUTCTime . fromIntegral . (`div` 1000) . ms
