{-# LANGUAGE StrictData #-}

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

-- | See also: "Galley.API.TeamNotifications".
--
-- This module is a clone of "Gundeck.Notification.Data".
--
-- FUTUREWORK: this is a work-around because it only solves *some* problems with team events.
-- We should really use a scalable message queue instead.
module Galley.Data.TeamNotifications
  ( ResultPage (..),
    add,
    fetch,
  )
where

import Cassandra as C
import qualified Data.Aeson as JSON
import Data.Id
import Data.List1 (List1)
import Data.Range (Range, fromRange)
import Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (><))
import qualified Data.Sequence as Seq
import Gundeck.Types.Notification
import Imports

data ResultPage = ResultPage
  { -- | A sequence of notifications.
    resultSeq :: Seq QueuedNotification,
    -- | Whether there might be more notifications that can be
    -- obtained through another query, starting the the ID of the
    -- last notification in 'resultSeq'.
    resultHasMore :: !Bool
  }

-- FUTUREWORK: the magic 32 should be made configurable, so it can be tuned
add ::
  (MonadClient m, MonadUnliftIO m) =>
  TeamId ->
  NotificationId ->
  List1 JSON.Object ->
  m ()
add tid nid (Blob . JSON.encode -> payload) =
  write cqlInsert (params Quorum (tid, nid, payload, notificationTTLSeconds)) & retry x5
  where
    cqlInsert :: PrepQuery W (TeamId, NotificationId, Blob, Int32) ()
    cqlInsert =
      "INSERT INTO team_notifications \
      \(team, id, payload) VALUES \
      \(?, ?, ?) \
      \USING TTL ?"

notificationTTLSeconds :: Int32
notificationTTLSeconds = 24192200

fetch :: forall m. MonadClient m => TeamId -> Maybe NotificationId -> Range 1 10000 Int32 -> m ResultPage
fetch tid since (fromRange -> size) = do
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  let size' = bool (+ 1) (+ 2) (isJust since) size
  page1 <- case TimeUuid . toUUID <$> since of
    Nothing -> paginate cqlStart (paramsP Quorum (Identity tid) size') & retry x1
    Just s -> paginate cqlSince (paramsP Quorum (tid, s) size') & retry x1
  -- Collect results, requesting more pages until we run out of data
  -- or have found size + 1 notifications (not including the 'since').
  let isize = fromIntegral size' :: Int
  (ns, more) <- collect Seq.empty isize page1
  -- Drop the extra element from the end as well.  Keep the inclusive start
  -- value in the response (if a 'since' was given and found).
  -- This can probably simplified a lot further, but we need to understand
  -- 'Seq' in order to do that.  If you find a bug, this may be a good
  -- place to start looking.
  return $! case Seq.viewl (trim (isize - 1) ns) of
    EmptyL -> ResultPage Seq.empty False
    (x :< xs) -> ResultPage (x <| xs) more
  where
    collect :: Seq QueuedNotification -> Int -> Page (TimeUuid, Blob) -> m (Seq QueuedNotification, Bool)
    collect acc num page =
      let ns = splitAt num $ foldr toNotif [] (result page)
          nseq = Seq.fromList (fst ns)
          more = hasMore page
          num' = num - Seq.length nseq
          acc' = acc >< nseq
       in if not more || num' == 0
            then return (acc', more || not (null (snd ns)))
            else liftClient (nextPage page) >>= collect acc' num'
    trim :: Int -> Seq a -> Seq a
    trim l ns
      | Seq.length ns <= l = ns
      | otherwise = case Seq.viewr ns of
        EmptyR -> ns
        xs :> _ -> xs
    cqlStart :: PrepQuery R (Identity TeamId) (TimeUuid, Blob)
    cqlStart =
      "SELECT id, payload \
      \FROM team_notifications \
      \WHERE team = ? \
      \ORDER BY id ASC"
    cqlSince :: PrepQuery R (TeamId, TimeUuid) (TimeUuid, Blob)
    cqlSince =
      "SELECT id, payload \
      \FROM team_notifications \
      \WHERE team = ? AND id >= ? \
      \ORDER BY id ASC"

-------------------------------------------------------------------------------
-- Conversions

toNotif :: (TimeUuid, Blob) -> [QueuedNotification] -> [QueuedNotification]
toNotif (i, b) ns =
  maybe
    ns
    (\p1 -> queuedNotification notifId p1 : ns)
    ( JSON.decode' (fromBlob b)
    -- FUTUREWORK: this is from the database, so it's slightly more ok to ignore parse
    -- errors than if it's data provided by a client.  it would still be better to have an
    -- error entry in the log file and crash, rather than ignore the error and continue.
    )
  where
    notifId = Id (fromTimeUuid i)
