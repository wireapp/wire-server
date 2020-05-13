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
-- The module is a clone of "Gundeck.Notification.Data".
--
-- FUTUREWORK: this is a work-around because it only solves *some* problems with team events.
-- We should really use a scalable message queue instead.
module Galley.Data.TeamNotifications
  ( ResultPage (..),
    add,
    fetch,
    fetchLast,
  )
where

import Cassandra as C
import Control.Lens ((^.), _1)
import qualified Data.Aeson as JSON
import Data.Id
import Data.List1 (List1)
import Data.Range (Range, fromRange)
import Data.Sequence ((<|), (><), Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence as Seq
import Gundeck.Types.Notification
import Imports

data ResultPage = ResultPage
  { -- | A sequence of notifications.
    resultSeq :: Seq QueuedNotification,
    -- | Whether there might be more notifications that can be
    -- obtained through another query, starting the the ID of the
    -- last notification in 'resultSeq'.
    resultHasMore :: !Bool,
    -- | Whether there might be a gap in the 'resultSeq'. This is 'True'
    -- iff a start ID ('since') has been given which could not be found.
    resultGap :: !Bool
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
      "INSERT INTO notifications \
      \(team, id, payload) VALUES \
      \(?, ?, ?) \
      \USING TTL ?"

notificationTTLSeconds :: Int32
notificationTTLSeconds = 24192200

fetchLast :: forall m. MonadClient m => TeamId -> m (Maybe QueuedNotification)
fetchLast t = do
  ls <- query cqlLast (params Quorum (Identity t)) & retry x1
  case ls of
    [] -> return Nothing
    ns@(n : _) -> ns `getFirstOrElse` do
      p <- paginate cqlSeek (paramsP Quorum (t, n ^. _1) 100) & retry x1
      seek p
  where
    seek ::
      Page (TimeUuid, Blob) ->
      m (Maybe QueuedNotification)
    seek p =
      result p
        `getFirstOrElse` if hasMore p
          then liftClient (nextPage p) >>= seek
          else return Nothing
    getFirstOrElse ::
      [(TimeUuid, Blob)] ->
      m (Maybe QueuedNotification) ->
      m (Maybe QueuedNotification)
    getFirstOrElse ns f =
      case listToMaybe (foldr' toNotif [] ns) of
        Just n -> return (Just n)
        Nothing -> f
    cqlLast :: PrepQuery R (Identity TeamId) (TimeUuid, Blob)
    cqlLast =
      "SELECT id, payload \
      \FROM notifications \
      \WHERE team = ? \
      \ORDER BY id DESC LIMIT 1"
    cqlSeek :: PrepQuery R (TeamId, TimeUuid) (TimeUuid, Blob)
    cqlSeek =
      "SELECT id, payload \
      \FROM notifications \
      \WHERE team = ? AND id < ? \
      \ORDER BY id DESC"

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
  return $! case Seq.viewl (trim (isize - 1) ns) of
    EmptyL -> ResultPage Seq.empty False (isJust since)
    (x :< xs) -> ResultPage (x <| xs) more (isJust since)
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
    trim l ns
      | Seq.length ns <= l = ns
      | otherwise = case Seq.viewr ns of
        EmptyR -> ns
        xs :> _ -> xs
    cqlStart :: PrepQuery R (Identity TeamId) (TimeUuid, Blob)
    cqlStart =
      "SELECT id, payload \
      \FROM notifications \
      \WHERE team = ? \
      \ORDER BY id ASC"
    cqlSince :: PrepQuery R (TeamId, TimeUuid) (TimeUuid, Blob)
    cqlSince =
      "SELECT id, payload \
      \FROM notifications \
      \WHERE team = ? AND id >= ? \
      \ORDER BY id ASC"

-------------------------------------------------------------------------------
-- Conversions

toNotif :: (TimeUuid, Blob) -> [QueuedNotification] -> [QueuedNotification]
toNotif (i, b) ns =
  maybe
    ns
    (\p1 -> queuedNotification notifId p1 : ns)
    (JSON.decode' (fromBlob b))
  where
    notifId = Id (fromTimeUuid i)
