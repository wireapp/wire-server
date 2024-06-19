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

module Gundeck.Notification
  ( paginate,
    PaginateResult (..),
  )
where

import Bilge.IO (post)
import Bilge.Request
import Bilge.Response
import Control.Lens (view)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (Milliseconds (..))
import Data.Range
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Data
import Gundeck.Options (brig)
import Imports hiding (getLast)
import Network.HTTP.Types (status400)
import Network.Wai.Utilities.Error
import System.Logger.Class
import System.Logger.Class qualified as Log
import Util.Options (Endpoint (Endpoint))
import Wire.API.Internal.Notification
import Wire.API.Notification

data PaginateResult = PaginateResult
  { paginateResultGap :: Bool,
    paginateResultPage :: QueuedNotificationList
  }

paginate :: UserId -> Maybe NotificationId -> Maybe ClientId -> Range 100 10000 Int32 -> Gundeck PaginateResult
paginate uid since mclt size = do
  traverse_ validateNotificationId since
  for_ mclt $ \clt -> updateActivity uid clt

  time <- posixTime
  rs <- Data.fetch uid mclt since size
  pure $ PaginateResult (Data.resultGap rs) (resultList time rs)
  where
    resultList time rs =
      queuedNotificationList
        (toList (Data.resultSeq rs))
        (Data.resultHasMore rs)
        (Just (millisToUTC time))
    millisToUTC = posixSecondsToUTCTime . fromIntegral . (`div` 1000) . ms

    validateNotificationId :: NotificationId -> Gundeck ()
    validateNotificationId n =
      unless (isValidNotificationId n) $
        throwM (mkError status400 "bad-request" "Invalid Notification ID")

-- | Update last_active property of the given client by making a request to brig.
updateActivity :: UserId -> ClientId -> Gundeck ()
updateActivity uid clt = do
  r <- do
    Endpoint h p <- view $ options . brig
    post
      ( host (toByteString' h)
          . port p
          . paths ["i", "clients", toByteString' uid, toByteString' clt, "activity"]
      )
  when (statusCode r /= 200) $ do
    Log.warn $
      Log.msg ("Could not update client activity" :: ByteString)
        ~~ "user"
        .= UUID.toASCIIBytes (toUUID uid)
        ~~ "client"
        .= clientToText clt
