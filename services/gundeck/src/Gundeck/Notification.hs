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

import Bilge.RPC
import Bilge.Request
import Bilge.Response
import Control.Lens (view)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Id
import Data.Range
import Data.UUID qualified as UUID
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Data
import Gundeck.Options (brig)
import Imports hiding (getLast)
import Network.HTTP.Types (status400)
import Network.HTTP.Types.Method
import Network.Wai.Utilities.Error
import System.Logger.Class
import System.Logger.Class qualified as Log
import Util.Options (Endpoint (Endpoint))
import Wire.API.Event.Transmit (transmitQueuedNotification)
import Wire.API.Internal.Notification
import Wire.API.Notification
import Wire.API.Routes.Version (Version)

data PaginateResult = PaginateResult
  { paginateResultGap :: Bool,
    paginateResultPage :: QueuedNotificationList
  }

paginate :: Version -> UserId -> Maybe NotificationId -> Maybe ClientId -> Range 100 10000 Int32 -> Gundeck PaginateResult
paginate v uid since mclt size = do
  traverse_ validateNotificationId since
  for_ mclt $ \clt -> updateActivity uid clt

  time <- posixTime
  rs <- Data.fetch uid mclt since size
  -- 'gap' semantics come from the first fetch only; pages fetched during the
  -- refill loop start exactly at the previous page's last cursor.
  (page, hasMore) <- refill rs (1 :: Int)
  pure $ PaginateResult (Data.resultGap rs) (resultList time hasMore page)
  where
    resultList time more ns =
      queuedNotificationList
        (toList ns)
        more
        (Just (msToUTCSecs time))

    -- Keep fetching while the client-visible survivors of the last page are
    -- empty but the store says there is more, so an all-gated page cannot
    -- leave the client's 'since' cursor stuck on an empty 'has_more=true'
    -- page.  Bounded to keep requests finite.
    refill rs pages = do
      let survivors = filtered rs
          more = Data.resultHasMore rs
      if not (null survivors) || not more
        then pure (survivors, more)
        else
          if pages >= refillPageLimit
            then do
              -- one survivor is enough to advance the client cursor, so refill
              -- pages are fetched at a small size to bound read amplification
              Log.warn $
                Log.msg (val "notification refill limit reached (all-gated backlog?)")
                  ~~ "user"
                  .= UUID.toASCIIBytes (toUUID uid)
              pure (survivors, more)
            else case listToMaybe (reverse (toList (Data.resultSeq rs))) of
              -- Nothing must stay terminal: 'hasMore' set on an empty page would
              -- otherwise loop forever.
              Nothing -> pure (survivors, more)
              Just lastRaw -> do
                rs' <- Data.fetch uid mclt (Just (view queuedNotificationId lastRaw)) refillSize
                refill rs' (pages + 1)

    filtered rs = mapMaybe (transmitQueuedNotification v) (toList (Data.resultSeq rs))

    refillSize = unsafeRange 100 :: Range 100 10000 Int32

    refillPageLimit = 32 :: Int

    validateNotificationId :: NotificationId -> Gundeck ()
    validateNotificationId n =
      unless (isValidNotificationId n) $
        throwM (mkError status400 "bad-request" "Invalid Notification ID")

-- | Update last_active property of the given client by making a request to brig.
updateActivity :: UserId -> ClientId -> Gundeck ()
updateActivity uid clt = do
  r <- do
    Endpoint h p <- view $ options . brig
    rpc "brig" $
      method POST
        . host (toByteString' h)
        . port p
        . paths ["i", "clients", toByteString' uid, toByteString' clt, "activity"]
  when (statusCode r /= 200) $ do
    Log.warn $
      Log.msg ("Could not update client activity" :: ByteString)
        ~~ "user"
        .= UUID.toASCIIBytes (toUUID uid)
        ~~ "client"
        .= clientToText clt
