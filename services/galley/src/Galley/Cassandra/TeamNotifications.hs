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

-- | See also: "Galley.API.TeamNotifications".
--
-- This module is a clone of "Gundeck.Notification.Data".
--
-- FUTUREWORK: this is a work-around because it only solves *some* problems with team events.
-- We should really use a scalable message queue instead.
module Galley.Cassandra.TeamNotifications
  ( interpretTeamNotificationStoreToCassandra,
  )
where

import Cassandra
import Control.Monad.Catch
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.Aeson qualified as JSON
import Data.Id
import Data.List1 (List1)
import Data.Range (Range, fromRange)
import Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (><))
import Data.Sequence qualified as Seq
import Data.Time (nominalDay, nominalDiffTimeToSeconds)
import Data.UUID.V1 qualified as UUID
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Data.TeamNotifications
import Galley.Effects
import Galley.Effects.TeamNotificationStore (TeamNotificationId, TeamNotificationStore (..))
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog hiding (err)
import Wire.API.Internal.Notification

interpretTeamNotificationStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (TeamNotificationStore ': r) a ->
  Sem r a
interpretTeamNotificationStoreToCassandra = interpret $ \case
  CreateTeamNotification tid nid objs -> do
    logEffect "TeamNotificationStore.CreateTeamNotification"
    embedClient $ add tid nid objs
  GetTeamNotifications tid mnid lim -> do
    logEffect "TeamNotificationStore.GetTeamNotifications"
    embedClient $ fetch tid mnid lim
  MkNotificationId -> do
    logEffect "TeamNotificationStore.MkNotificationId"
    embed mkNotificationId

-- | 'Data.UUID.V1.nextUUID' is sometimes unsuccessful, so we try a few times.
mkNotificationId :: IO TeamNotificationId
mkNotificationId = do
  ni <- fmap Id <$> retrying x10 fun (const (liftIO UUID.nextUUID))
  maybe (throwM err) pure ni
  where
    x10 = limitRetries 10 <> exponentialBackoff 10
    fun = const (pure . isNothing)
    err = mkError status500 "internal-error" "unable to generate notification ID"

-- FUTUREWORK: the magic 32 should be made configurable, so it can be tuned
add ::
  TeamId ->
  TeamNotificationId ->
  List1 JSON.Object ->
  Client ()
add tid nid (Blob . JSON.encode -> payload) =
  write cqlInsert (params LocalQuorum (tid, nid, payload, notificationTTLSeconds)) & retry x5
  where
    cqlInsert :: PrepQuery W (TeamId, TeamNotificationId, Blob, Int32) ()
    cqlInsert =
      "INSERT INTO team_notifications \
      \(team, id, payload) VALUES \
      \(?, ?, ?) \
      \USING TTL ?"

-- |
--
-- >>> import Data.Time
-- >>> formatTime defaultTimeLocale "%d days, %H hours, %M minutes, %S seconds" (secondsToNominalDiffTime (fromIntegral notificationTTLSeconds))
-- "28 days, 0 hours, 0 minutes, 0 seconds"
notificationTTLSeconds :: Int32
notificationTTLSeconds = round $ nominalDiffTimeToSeconds $ 28 * nominalDay

fetch :: TeamId -> Maybe TeamNotificationId -> Range 1 10000 Int32 -> Client ResultPage
fetch tid since (fromRange -> size) = do
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  let size' = bool (+ 1) (+ 2) (isJust since) size
  page1 <- case TimeUuid . toUUID <$> since of
    Nothing -> paginate cqlStart (paramsP LocalQuorum (Identity tid) size') & retry x1
    Just s -> paginate cqlSince (paramsP LocalQuorum (tid, s) size') & retry x1
  -- Collect results, requesting more pages until we run out of data
  -- or have found size + 1 notifications (not including the 'since').
  let isize = fromIntegral size' :: Int
  (ns, more) <- collect Seq.empty isize page1
  -- Drop the extra element from the end as well.  Keep the inclusive start
  -- value in the response (if a 'since' was given and found).
  -- This can probably simplified a lot further, but we need to understand
  -- 'Seq' in order to do that.  If you find a bug, this may be a good
  -- place to start looking.
  pure $! case Seq.viewl (trim (isize - 1) ns) of
    EmptyL -> ResultPage Seq.empty False
    (x :< xs) -> ResultPage (x <| xs) more
  where
    collect ::
      Seq QueuedNotification ->
      Int ->
      Page (TimeUuid, Blob) ->
      Client (Seq QueuedNotification, Bool)
    collect acc num page =
      let ns = splitAt num $ foldr toNotif [] (result page)
          nseq = Seq.fromList (fst ns)
          more = hasMore page
          num' = num - Seq.length nseq
          acc' = acc >< nseq
       in if not more || num' == 0
            then pure (acc', more || not (null (snd ns)))
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
    notifId = idToText $ Id (fromTimeUuid i)
