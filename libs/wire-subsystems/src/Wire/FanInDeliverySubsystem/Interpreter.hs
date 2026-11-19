{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Session-local state machine. The adapter serializes operations for each
-- socket independently; no lock or cursor is shared between sockets.
module Wire.FanInDeliverySubsystem.Interpreter
  ( interpretFanInDeliverySubsystem,
    FanInDeliveryState,
    emptyFanInDeliveryState,
  )
where

import Data.Aeson qualified as A
import Data.Domain (Domain)
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Qualified (Qualified (..))
import Data.Range
import Data.Set qualified as Set
import Data.UUID.V4 qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Wire.API.Event.WebSocketProtocol (AckData (..), EventData (..))
import Wire.API.MLS.Epoch (Epoch)
import Wire.API.MLS.Group (GroupId)
import Wire.API.Notification (NotificationId, queuedNotification)
import Wire.API.Notification.FanIn
import Wire.FanInAckStore
import Wire.FanInDeliveryAccess
import Wire.FanInDeliverySubsystem
import Wire.FanInEpochMembershipStore
import Wire.FanInNotificationStore

-- Stream identities are private to delivery. Store operations remain typed.
data Stream = UserStream UserId | ClientStream UserId ClientId | TeamStream TeamId | EpochStream GroupId Epoch | LocalStream UserId | RemoteStream Domain UserId
  deriving stock (Eq, Ord, Show)

data StreamState = StreamState {ackKeys :: [FanInAckKey], cursor :: Maybe NotificationId}

data Pending = Pending {stream :: Stream, notificationId :: NotificationId, acknowledged :: Bool}

data Session = Session
  { userId :: UserId,
    clientId :: Maybe ClientId,
    streams :: Map Stream StreamState,
    pending :: Map Word64 Pending,
    nextTag :: Word64,
    missed :: Bool,
    missedReported :: Bool,
    synchronized :: Bool,
    turn :: Int
  }

type FanInDeliveryState = Map FanInSessionId Session

emptyFanInDeliveryState :: FanInDeliveryState
emptyFanInDeliveryState = Map.empty

type DeliveryConstraints r = (Member FanInDeliveryAccess r, Member FanInNotificationStore r, Member FanInAckStore r, Member FanInEpochMembershipStore r, Member (State FanInDeliveryState) r, Member (Error FanInDeliveryError) r, Member (Embed IO) r)

interpretFanInDeliverySubsystem :: forall r. (DeliveryConstraints r) => Domain -> InterpreterFor FanInDeliverySubsystem r
interpretFanInDeliverySubsystem localDomain = interpret $ \case
  OpenFanInSession uid cid -> do
    sid <- FanInSessionId <$> liftIO UUID.nextRandom
    session <- refresh localDomain (Session uid cid Map.empty Map.empty 1 False False False 0)
    modify (Map.insert sid session)
    pure sid
  ReadFanInSession sid pageSize -> do
    original <- findSession sid
    session <- refresh localDomain original
    if session.missed
      then do
        save sid session {missedReported = True}
        pure [FanInNotificationsMissed | not session.missedReported]
      else do
        -- Count acknowledged-but-blocked entries too: otherwise acknowledging
        -- later tags could grow memory without bound behind one missing ack.
        let capacity = max 0 (min 100 (fromIntegral (fromRange pageSize)) - Map.size session.pending)
            ordered = Map.keys session.streams
            offset = if null ordered then 0 else session.turn `mod` length ordered
            keys = drop offset ordered <> take offset ordered
        (updated, events, more) <- fetchStreams capacity session keys
        let marker = not updated.synchronized && not more
        save sid updated {synchronized = updated.synchronized || marker, turn = session.turn + 1}
        pure (events <> [FanInSynchronization | marker])
  HandleFanInClientMessage sid message -> do
    original <- findSession sid
    session <- refresh localDomain original
    case message of
      FanInAckFullSync -> do
        unless session.missed $ throw FanInUnexpectedFullSyncAck
        states <- for (Map.toList session.streams) $ \(key, state) -> do
          headId <- lastId key
          boundary <- maybe createFanInEmptyCursor pure headId
          for_ state.ackKeys $ \ackKey -> advanceFanInAck ackKey boundary
          pure (key, state {cursor = Just boundary})
        save sid session {streams = Map.fromList states, pending = Map.empty, missed = False, missedReported = False, synchronized = False}
      FanInAck ack -> do
        when (ack.deliveryTag == 0 || ack.deliveryTag >= session.nextTag) $ throw FanInInvalidDeliveryTag
        -- Old/duplicate tags are harmless; future tags are never accepted.
        unless session.missed $ do
          let marked = Map.mapWithKey (\tag entry -> entry {acknowledged = entry.acknowledged || tag == ack.deliveryTag || (ack.multiple && tag <= ack.deliveryTag)}) session.pending
          remaining <- foldM (commitPrefix session.streams) marked (Map.keys session.streams)
          save sid session {pending = remaining}
  DeliverFanInTransient sid push -> do
    session <- findSession sid >>= refresh localDomain
    -- Recheck current membership for transient events too. These events do not
    -- consume durable acknowledgement state or the persistent delivery window.
    let allowed = any (targetAllowed localDomain session.streams session.userId) (NE.toList push.targets)
    if not push.transient || session.missed || not allowed
      then pure Nothing
      else do
        let tag = session.nextTag
        when (tag == maxBound) $ throw FanInInvalidDeliveryTag
        save sid session {nextTag = tag + 1}
        pure $ Just (FanInEvent (EventData (queuedNotification push.notificationId (push.json NE.:| [])) tag))
  NotifyFanInSessions request -> notifyFanInSockets request
  CloseFanInSession sid -> modify (Map.delete sid)

findSession :: (DeliveryConstraints r) => FanInSessionId -> Sem r Session
findSession sid = gets (Map.lookup sid) >>= maybe (throw FanInUnknownSession) pure

save :: (Member (State FanInDeliveryState) r) => FanInSessionId -> Session -> Sem r ()
save sid session = modify (Map.insert sid session)

-- Deduplicate shared streams when no client was selected. Deliver each event
-- once, with acknowledgements advancing all eligible clients' durable cursors.
refresh :: (DeliveryConstraints r) => Domain -> Session -> Sem r Session
refresh localDomain session = do
  registered <- getFanInClientIds session.userId
  clients <- case session.clientId of
    Just cid -> if cid `elem` registered then pure [cid] else throw FanInClientNotOwned
    Nothing -> pure registered
  teams <- getFanInTeamIds session.userId
  connections <- getFanInAcceptedConnections session.userId
  epochs <- fmap concat $ for clients $ \cid -> do
    refs <- getFanInEpochsForClient session.userId cid
    pure [(EpochStream ref.fanInEpochRefGroupId ref.fanInEpochRefEpoch, [FanInEpochAck session.userId cid ref.fanInEpochRefGroupId ref.fanInEpochRefEpoch]) | ref <- refs]
  let uid = session.userId
      scopes =
        [(UserStream uid, [FanInUserAck uid cid | cid <- clients])]
          <> [(ClientStream uid cid, [FanInClientAck uid cid]) | cid <- clients]
          <> [(TeamStream tid, [FanInTeamAck uid cid tid | cid <- clients]) | tid <- teams]
          <> epochs
          <> [(if domain == localDomain then LocalStream connected else RemoteStream domain connected, [if domain == localDomain then FanInLocalConnectionAck uid connected else FanInRemoteConnectionAck uid domain connected]) | Qualified connected domain <- connections]
      authorized = Map.fromListWith (<>) scopes
  states <- for (Map.toList authorized) $ \(key, ackKeys) -> do
    case Map.lookup key session.streams of
      Just state | Set.fromList state.ackKeys == Set.fromList ackKeys -> pure (key, state)
      _ -> do
        headId <- lastId key
        acknowledged <- traverse getFanInAck ackKeys
        boundary <- if any isNothing acknowledged then Just <$> maybe createFanInEmptyCursor pure headId else pure headId
        cursors <- for (zip ackKeys acknowledged) $ \(ackKey, previous) -> case previous of
          Just nid -> pure (Just nid)
          Nothing -> traverse (initializeFanInAck ackKey) boundary
        pure (key, StreamState ackKeys (if null cursors then headId else minimum cursors))
  let streams = Map.fromList states
      eligible =
        Map.filter
          ( \entry -> case (Map.lookup entry.stream streams, Map.lookup entry.stream session.streams) of
              (Just now, Just old) -> now.ackKeys == old.ackKeys
              _ -> False
          )
          session.pending
  gaps <- for (Map.toList streams) $ \(key, state) -> case state.cursor of
    Nothing -> pure False
    Just nid -> do
      headId <- lastId key
      -- Heads outlive event partitions. No head means this stream has never
      -- had an event, so expiration of its empty boundary alone is not a gap.
      if isNothing headId || headId == Just nid
        then pure False
        else do
          eventRetained <- contains key nid
          boundaryRetained <- if eventRetained then pure False else hasFanInEmptyCursor nid
          pure (not (eventRetained || boundaryRetained))
  pure session {streams, pending = eligible, missed = session.missed || or gaps}

fetchStreams :: (DeliveryConstraints r) => Int -> Session -> [Stream] -> Sem r (Session, [FanInServerMessage], Bool)
fetchStreams _ session [] = pure (session, [], False)
fetchStreams capacity session keys@(key : rest)
  | capacity <= 0 = pure (session, [], not (null keys))
  | otherwise = do
      state <- maybe (throw FanInUnknownSession) pure (Map.lookup key session.streams)
      page <- pageAfter key state.cursor (unsafeRange (fromIntegral capacity))
      (sent, events) <- foldM (emit key) (session, []) page.fanInNotifications
      let latest = case reverse page.fanInNotifications of
            n : _ -> Just n.fanInNotificationId
            [] -> state.cursor
          next = sent {streams = Map.insert key state {cursor = latest} sent.streams}
      (final, later, more) <- fetchStreams (capacity - length events) next rest
      pure (final, events <> later, page.fanInNotificationsHaveMore || more)

emit :: (DeliveryConstraints r) => Stream -> (Session, [FanInServerMessage]) -> FanInNotification -> Sem r (Session, [FanInServerMessage])
emit key (session, events) notification = do
  payload <- case A.fromJSON notification.fanInNotificationPayload of
    A.Success payload -> pure payload
    A.Error _ -> throw FanInInvalidStoredPayload
  let tag = session.nextTag
  when (tag == maxBound) $ throw FanInInvalidDeliveryTag
  let event = FanInEvent (EventData (queuedNotification notification.fanInNotificationId payload) tag)
      pending = Map.insert tag (Pending key notification.fanInNotificationId False) session.pending
  pure (session {pending, nextTag = tag + 1}, events <> [event])

-- Persist only the contiguous acknowledged prefix within each stream, even
-- when tags from independent streams are interleaved or acked out of order.
commitPrefix :: (DeliveryConstraints r) => Map Stream StreamState -> Map Word64 Pending -> Stream -> Sem r (Map Word64 Pending)
commitPrefix states entries key = do
  let prefix = takeWhile (\(_, p) -> p.acknowledged) [(tag, p) | (tag, p) <- Map.toAscList entries, p.stream == key]
  case reverse prefix of
    [] -> pure entries
    (_, latest) : _ -> do
      for_ (Map.lookup key states) $ \state -> for_ state.ackKeys $ \ackKey -> advanceFanInAck ackKey latest.notificationId
      pure $ foldr (Map.delete . fst) entries prefix

lastId :: (Member FanInNotificationStore r) => Stream -> Sem r (Maybe NotificationId)
lastId = \case
  UserStream uid -> getLastFanInUserNotificationId uid
  ClientStream uid cid -> getLastFanInClientNotificationId uid cid
  TeamStream tid -> getLastFanInTeamNotificationId tid
  EpochStream gid epoch -> getLastFanInEpochNotificationId gid epoch
  LocalStream uid -> getLastFanInLocalConnectionNotificationId uid
  RemoteStream domain uid -> getLastFanInRemoteConnectionNotificationId domain uid

contains :: (Member FanInNotificationStore r) => Stream -> NotificationId -> Sem r Bool
contains key nid = case key of
  UserStream uid -> hasFanInUserNotification uid nid
  ClientStream uid cid -> hasFanInClientNotification uid cid nid
  TeamStream tid -> hasFanInTeamNotification tid nid
  EpochStream gid epoch -> hasFanInEpochNotification gid epoch nid
  LocalStream uid -> hasFanInLocalConnectionNotification uid nid
  RemoteStream domain uid -> hasFanInRemoteConnectionNotification domain uid nid

pageAfter :: (Member FanInNotificationStore r) => Stream -> Maybe NotificationId -> Range 1 10000 Int32 -> Sem r FanInNotificationPage
pageAfter key = case key of
  UserStream uid -> getFanInUserNotificationPage uid
  ClientStream uid cid -> getFanInClientNotificationPage uid cid
  TeamStream tid -> getFanInTeamNotificationPage tid
  EpochStream gid epoch -> getFanInEpochNotificationPage gid epoch
  LocalStream uid -> getFanInLocalConnectionNotificationPage uid
  RemoteStream domain uid -> getFanInRemoteConnectionNotificationPage domain uid

targetAllowed :: Domain -> Map Stream StreamState -> UserId -> FanInTarget -> Bool
targetAllowed localDomain streams uid = \case
  FanInTargetUser target -> uid == target
  FanInTargetUserClients target clients -> uid == target && any (\cid -> Map.member (ClientStream uid cid) streams) clients
  FanInTargetTeam tid -> Map.member (TeamStream tid) streams
  FanInTargetEpoch gid epoch -> Map.member (EpochStream gid epoch) streams
  FanInTargetConnections (Qualified connected domain) -> Map.member (if domain == localDomain then LocalStream connected else RemoteStream domain connected) streams
