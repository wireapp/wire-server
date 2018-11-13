{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Gundeck.Notification.Data
    ( ResultPage (..)
    , add
    , fetch
    , fetchId
    , fetchLast
    , deleteAll
    ) where

import Imports
import Cassandra as C
import Control.Lens ((^.), _1)
import Data.Id
import Data.List1 (List1)
import Data.Range (Range, fromRange)
import Data.Sequence (Seq, (><), (<|), ViewL (..), ViewR (..))
import Gundeck.Types.Notification
import Gundeck.Options (NotificationTTL (..))
import UnliftIO (mapConcurrently)

import qualified Data.Aeson      as JSON
import qualified Data.List.Extra as List
import qualified Data.Sequence   as Seq

data ResultPage = ResultPage
    { resultSeq :: Seq QueuedNotification
        -- ^ A sequence of notifications.
    , resultHasMore :: !Bool
        -- ^ Whether there might be more notifications that can be
        -- obtained through another query, starting the the ID of the
        -- last notification in 'resultSeq'.
    , resultGap :: !Bool
        -- ^ Whether there might be a gap in the 'resultSeq'. This is 'True'
        -- iff a start ID ('since') has been given which could not be found.
    }

add :: (MonadClient m, MonadUnliftIO m)
    => NotificationId
    -> List1 NotificationTarget
    -> List1 JSON.Object
    -> NotificationTTL
    -> m ()
add n (List.chunksOf 32 . toList -> tgts) (Blob . JSON.encode -> p) (notificationTTLSeconds -> t) =
    forM_ tgts $ mapConcurrently $ \tgt ->
        let u  = tgt^.targetUser
            cs = C.Set (tgt^.targetClients)
        in write cqlInsert (params Quorum (u, n, p, cs, fromIntegral t)) & retry x5
  where
    cqlInsert :: PrepQuery W (UserId, NotificationId, Blob, C.Set ClientId, Int32) ()
    cqlInsert =
        "INSERT INTO notifications \
        \(user, id, payload, clients) VALUES \
        \(?   , ? , ?      , ?) \
        \USING TTL ?"

fetchId :: MonadClient m => UserId -> NotificationId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchId u n c = listToMaybe . foldr' (toNotif c) [] <$>
    query cqlById (params Quorum (u, n)) & retry x1
  where
    cqlById :: PrepQuery R (UserId, NotificationId) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlById =
        "SELECT id, payload, clients \
        \FROM notifications \
        \WHERE user = ? AND id = ?"

fetchLast :: MonadClient m => UserId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchLast u c = do
    ls <- query cqlLast (params Quorum (Identity u)) & retry x1
    case ls of
        []       -> return Nothing
        ns@(n:_) -> ns `getFirstOrElse` do
            p <- paginate cqlSeek (paramsP Quorum (u, n^._1) 100) & retry x1
            seek p
  where
    seek p = result p `getFirstOrElse`
        if hasMore p
            then liftClient (nextPage p) >>= seek
            else return Nothing

    getFirstOrElse ns f =
        case listToMaybe (foldr' (toNotif c) [] ns) of
            Just  n -> return (Just n)
            Nothing -> f

    cqlLast :: PrepQuery R (Identity UserId) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlLast =
        "SELECT id, payload, clients \
        \FROM notifications \
        \WHERE user = ? \
        \ORDER BY id DESC LIMIT 1"

    cqlSeek :: PrepQuery R (UserId, TimeUuid) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlSeek =
        "SELECT id, payload, clients \
        \FROM notifications \
        \WHERE user = ? AND id < ? \
        \ORDER BY id DESC"

fetch :: MonadClient m => UserId -> Maybe ClientId -> Maybe NotificationId -> Range 100 10000 Int32 -> m ResultPage
fetch u c since (fromRange -> size) = do
    -- We always need to look for one more than requested in order to correctly
    -- report whether there are more results.
    let size' = bool (+1) (+2) (isJust since) size
    page1 <- case TimeUuid . toUUID <$> since of
        Nothing -> paginate cqlStart (paramsP Quorum (Identity u) size') & retry x1
        Just  s -> paginate cqlSince (paramsP Quorum (u, s) size') & retry x1

    -- Collect results, requesting more pages until we run out of data
    -- or have found size + 1 notifications (not including the 'since').
    let isize = fromIntegral size' :: Int
    (ns, more) <- collect Seq.empty isize page1

    -- Drop the extra element from the end as well as the inclusive start
    -- value (if a 'since' was given and found).
    return $! case Seq.viewl (trim (isize - 1) ns) of
        EmptyL  -> ResultPage Seq.empty False (isJust since)
        x :< xs -> case since of
            Just s | s == x^.queuedNotificationId
                -> ResultPage xs more False
            _   -> ResultPage (x <| xs) more (isJust since)
  where
    collect acc num page =
        let ns   = splitAt num $ foldr (toNotif c) [] (result page)
            nseq = Seq.fromList (fst ns)
            more = hasMore page
            num' = num - Seq.length nseq
            acc' = acc >< nseq
        in if not more || num' == 0
            then return (acc', more || not (null (snd ns)))
            else liftClient (nextPage page) >>= collect acc' num'

    trim l ns | Seq.length ns <= l = ns
              | otherwise = case Seq.viewr ns of
                                EmptyR  -> ns
                                xs :> _ -> xs

    cqlStart :: PrepQuery R (Identity UserId) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlStart =
        "SELECT id, payload, clients \
        \FROM notifications \
        \WHERE user = ? \
        \ORDER BY id ASC"

    cqlSince :: PrepQuery R (UserId, TimeUuid) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlSince =
        "SELECT id, payload, clients \
        \FROM notifications \
        \WHERE user = ? AND id >= ? \
        \ORDER BY id ASC"

deleteAll :: MonadClient m => UserId -> m ()
deleteAll u = write cql (params Quorum (Identity u)) & retry x5
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM notifications WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

toNotif :: Maybe ClientId
        -> (TimeUuid, Blob, Maybe (C.Set ClientId))
        -> [QueuedNotification]
        -> [QueuedNotification]
toNotif c (i, b, cs) ns =
    let clients = maybe [] fromSet cs
        notifId = Id (fromTimeUuid i)
    in case JSON.decode' (fromBlob b) of
        Nothing -> ns
        Just pl ->
            -- nb. At some point we should be able to do:
            -- @@@ if null clients || maybe False (`elem` clients) c @@@
            -- i.e. not return notifications targeted at specific clients,
            -- if no client ID is given. We currently return all of them
            -- in this case for backward compatibility with existing internal
            -- clients.
            if null clients || maybe True (`elem` clients) c
                then queuedNotification notifId pl : ns
                else ns
