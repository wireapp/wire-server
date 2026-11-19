-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FanInDeliverySubsystem.InterpreterSpec (spec) where

import Control.Lens (view)
import Data.Aeson qualified as A
import Data.Domain (Domain (..))
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Qualified (Qualified (..))
import Data.Range
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Test.Hspec
import Wire.API.Event.WebSocketProtocol (AckData (..), EventData (..))
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.Notification (NotificationId, queuedNotificationId)
import Wire.API.Notification.FanIn
import Wire.API.Push.V2 (Route (..))
import Wire.FanInAckStore
import Wire.FanInDeliveryAccess
import Wire.FanInDeliverySubsystem
import Wire.FanInDeliverySubsystem.Interpreter
import Wire.FanInEpochMembershipStore
import Wire.FanInNotificationStore

data Scope = U UserId | C UserId ClientId | T TeamId | E GroupId Epoch | L UserId | R Domain UserId
  deriving stock (Eq, Ord, Show)

data Model = Model
  { rows :: Map Scope [FanInNotification],
    heads :: Map Scope NotificationId,
    markers :: [NotificationId],
    acks :: Map FanInAckKey NotificationId,
    clients :: [ClientId],
    teams :: [TeamId],
    connections :: [Qualified UserId],
    epochs :: Map ClientId [FanInEpochRef]
  }

type TestEffects = '[FanInDeliverySubsystem, State FanInDeliveryState, FanInDeliveryAccess, FanInNotificationStore, FanInAckStore, FanInEpochMembershipStore, Error FanInDeliveryError, State Model, Embed IO]

spec :: Spec
spec = describe "Fan-in delivery" $ do
  it "initializes at the head, sends new events once, and resumes after acknowledgement" $ do
    (model, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      addRows (U uid) [2]
      first <- readFanInSession sid (unsafeRange 100)
      repeated <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 1 False))
      closeFanInSession sid
      reopened <- openFanInSession uid (Just cid)
      resumed <- readFanInSession reopened (unsafeRange 100)
      pure (eventIds first, FanInSynchronization `elem` first, repeated, resumed)
    snd <$> result `shouldBe` Right ([nid 2], True, [], [FanInSynchronization])
    Map.lookup (FanInUserAck uid cid) model.acks `shouldBe` Just (nid 2)
  it "replays the first event after reconnecting from an initially empty stream" $ do
    (_, result) <- runScenario initial {rows = Map.empty, heads = Map.empty} $ do
      sid <- openFanInSession uid (Just cid)
      _ <- readFanInSession sid (unsafeRange 100)
      addRows (U uid) [2]
      first <- readFanInSession sid (unsafeRange 100)
      closeFanInSession sid
      next <- openFanInSession uid (Just cid)
      replay <- readFanInSession next (unsafeRange 100)
      pure (eventIds first, eventIds replay)
    snd <$> result `shouldBe` Right ([nid 2], [nid 2])
  it "does not advance beyond a gap when single acknowledgements arrive out of order" $ do
    (_, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      addRows (U uid) [2, 3]
      _ <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 2 False))
      cursorBefore <- gets @Model (Map.lookup (FanInUserAck uid cid) . acks)
      handleFanInClientMessage sid (FanInAck (AckData 1 False))
      cursorAfter <- gets @Model (Map.lookup (FanInUserAck uid cid) . acks)
      pure (cursorBefore, cursorAfter)
    snd <$> result `shouldBe` Right (Just (nid 1), Just (nid 3))
  it "replays unacknowledged events after a reconnect" $ do
    (_, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      addRows (U uid) [2, 3]
      sent <- readFanInSession sid (unsafeRange 100)
      closeFanInSession sid
      new <- openFanInSession uid (Just cid)
      replayed <- readFanInSession new (unsafeRange 100)
      pure (eventIds sent, eventIds replayed)
    snd <$> result `shouldBe` Right ([nid 2, nid 3], [nid 2, nid 3])
  it "handles multiple acknowledgements across independently ordered streams" $ do
    (model, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      addRows (U uid) [2, 3]
      addRows (C uid cid) [4, 5]
      events <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 3 True))
      pure (eventIds events)
    snd <$> result `shouldBe` Right [nid 2, nid 3, nid 4, nid 5]
    Map.lookup (FanInUserAck uid cid) model.acks `shouldBe` Just (nid 3)
    Map.lookup (FanInClientAck uid cid) model.acks `shouldBe` Just (nid 4)
  it "rejects future delivery tags without advancing durable cursors" $ do
    (model, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      addRows (U uid) [2]
      _ <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 2 True))
    either Just (const Nothing) result `shouldBe` Just FanInInvalidDeliveryTag
    Map.lookup (FanInUserAck uid cid) model.acks `shouldBe` Just (nid 1)
  it "bounds outstanding state even when the final tag is acknowledged first" $ do
    (_, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      addRows (U uid) [2 .. 151]
      first <- readFanInSession sid (unsafeRange 10000)
      handleFanInClientMessage sid (FanInAck (AckData 100 False))
      blocked <- readFanInSession sid (unsafeRange 10000)
      handleFanInClientMessage sid (FanInAck (AckData 1 False))
      next <- readFanInSession sid (unsafeRange 10000)
      pure (length (eventIds first), eventIds blocked, eventIds next)
    snd <$> result `shouldBe` Right (100, [], [nid 102])
  it "signals an expired cursor once and resumes only after full-sync acknowledgement" $ do
    let expired = initial {rows = Map.singleton (U uid) [notification 2, notification 3], heads = Map.singleton (U uid) (nid 3), acks = Map.singleton (FanInUserAck uid cid) (nid 1)}
    (model, result) <- runScenario expired $ do
      sid <- openFanInSession uid (Just cid)
      missed <- readFanInSession sid (unsafeRange 100)
      waiting <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid FanInAckFullSync
      resumed <- readFanInSession sid (unsafeRange 100)
      pure (missed, waiting, resumed)
    snd <$> result `shouldBe` Right ([FanInNotificationsMissed], [], [FanInSynchronization])
    Map.lookup (FanInUserAck uid cid) model.acks `shouldBe` Just (nid 3)
  it "does not require full sync just because a never-used stream's boundary expired" $ do
    (_, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      _ <- readFanInSession sid (unsafeRange 100)
      modify @Model (\m -> m {markers = []})
      readFanInSession sid (unsafeRange 100)
    snd <$> result `shouldBe` Right []
  it "detects expiry of an empty-stream boundary without delivering it as an event" $ do
    (_, result) <- runScenario initial {rows = Map.empty, heads = Map.empty} $ do
      sid <- openFanInSession uid (Just cid)
      _ <- readFanInSession sid (unsafeRange 100)
      addRows (U uid) [2]
      modify @Model (\m -> m {markers = []})
      missed <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid FanInAckFullSync
      resumed <- readFanInSession sid (unsafeRange 100)
      pure (missed, resumed)
    snd <$> result `shouldBe` Right ([FanInNotificationsMissed], [FanInSynchronization])
  it "accepts an expired cursor that still equals the stream head" $ do
    (_, result) <- runScenario initial {rows = Map.empty, acks = Map.singleton (FanInUserAck uid cid) (nid 1)} $ do
      sid <- openFanInSession uid (Just cid)
      readFanInSession sid (unsafeRange 100)
    snd <$> result `shouldBe` Right [FanInSynchronization]
  it "rejects unsolicited full-sync acknowledgements" $ do
    (_, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      handleFanInClientMessage sid FanInAckFullSync
    either Just (const Nothing) result `shouldBe` Just FanInUnexpectedFullSyncAck
  it "requires the requested client to belong to the authenticated user" $ do
    (_, result) <- runScenario initial (openFanInSession uid (Just (ClientId 99)))
    either Just (const Nothing) result `shouldBe` Just FanInClientNotOwned
  it "delivers a shared stream once when all clients are selected" $ do
    (model, result) <- runScenario initial {clients = [cid, ClientId 2]} $ do
      sid <- openFanInSession uid Nothing
      addRows (U uid) [2]
      events <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 1 False))
      pure (eventIds events)
    snd <$> result `shouldBe` Right [nid 2]
    for_ [cid, ClientId 2] $ \client -> Map.lookup (FanInUserAck uid client) model.acks `shouldBe` Just (nid 2)
  it "drops revoked team access before reading or persisting outstanding acknowledgements" $ do
    let tid = Id UUID.nil
    (model, result) <- runScenario initial {teams = [tid]} $ do
      sid <- openFanInSession uid (Just cid)
      addRows (T tid) [2]
      sent <- readFanInSession sid (unsafeRange 100)
      modify @Model (\m -> m {teams = []})
      handleFanInClientMessage sid (FanInAck (AckData 1 False))
      addRows (T tid) [3]
      afterRemoval <- readFanInSession sid (unsafeRange 100)
      pure (eventIds sent, eventIds afterRemoval)
    snd <$> result `shouldBe` Right ([nid 2], [])
    Map.lookup (FanInTeamAck uid cid tid) model.acks `shouldBe` Just (nid 0)

  it "reads retained epoch streams and advances only the owning client's epoch cursor" $ do
    let gid = GroupId "group"
        epoch = Epoch 7
        member = FanInEpochRef gid epoch
    (model, result) <- runScenario initial {epochs = Map.singleton cid [member]} $ do
      sid <- openFanInSession uid (Just cid)
      addRows (E gid epoch) [2, 3]
      received <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 2 True))
      pure (eventIds received)
    snd <$> result `shouldBe` Right [nid 2, nid 3]
    Map.lookup (FanInEpochAck uid cid gid epoch) model.acks `shouldBe` Just (nid 3)
    Map.lookup (FanInEpochAck uid (ClientId 2) gid epoch) model.acks `shouldBe` Nothing
  it "reads local and remote connection streams, then drops blocked connections" $ do
    let localDomain = Domain "local.example.com"
        remote = Domain "remote.example.com"
        other = Id (UUID.fromWords 0 0 0 2)
    (model, result) <- runScenario initial {connections = [Qualified other localDomain, Qualified other remote]} $ do
      sid <- openFanInSession uid (Just cid)
      addRows (L other) [2]
      addRows (R remote other) [3]
      received <- readFanInSession sid (unsafeRange 100)
      handleFanInClientMessage sid (FanInAck (AckData 2 True))
      modify @Model (\m -> m {connections = []})
      addRows (L other) [4]
      addRows (R remote other) [5]
      blocked <- readFanInSession sid (unsafeRange 100)
      pure (eventIds received, eventIds blocked)
    snd <$> result `shouldBe` Right ([nid 2, nid 3], [])
    Map.lookup (FanInLocalConnectionAck uid other) model.acks `shouldBe` Just (nid 2)
    Map.lookup (FanInRemoteConnectionAck uid remote other) model.acks `shouldBe` Just (nid 3)
  it "never forwards a remote transient because a local connection has the same user id" $ do
    let other = Id (UUID.fromWords 0 0 0 2)
        localDomain = Domain "local.example.com"
        remote = Domain "remote.example.com"
        push = transientPush (FanInTargetConnections (Qualified other remote))
    (_, result) <- runScenario initial {connections = [Qualified other localDomain]} $ do
      sid <- openFanInSession uid (Just cid)
      deliverFanInTransient sid push
    snd <$> result `shouldBe` Right Nothing
  it "transient delivery shares tags but never advances durable acknowledgement state" $ do
    (model, result) <- runScenario initial $ do
      sid <- openFanInSession uid (Just cid)
      event <- deliverFanInTransient sid (transientPush (FanInTargetUser uid))
      handleFanInClientMessage sid (FanInAck (AckData 1 False))
      pure (eventIds (toList event))
    snd <$> result `shouldBe` Right [nid 2]
    Map.lookup (FanInUserAck uid cid) model.acks `shouldBe` Just (nid 1)

transientPush :: FanInTarget -> FanInPush
transientPush target = FanInPush (nid 2) (target :| []) Nothing True RouteAny Nothing Nothing mempty Nothing False

runScenario :: Model -> Sem TestEffects a -> IO (Model, Either FanInDeliveryError (FanInDeliveryState, a))
runScenario model = runM . runState model . runError . fakeEpochs . fakeAcks . fakeNotifications . fakeAccess . runState emptyFanInDeliveryState . interpretFanInDeliverySubsystem (Domain "local.example.com")

fakeAccess :: (Member (State Model) r) => InterpreterFor FanInDeliveryAccess r
fakeAccess = interpret $ \case
  GetFanInClientIds _ -> gets @Model clients
  GetFanInTeamIds _ -> gets @Model teams
  GetFanInAcceptedConnections _ -> gets @Model connections
  NotifyFanInSockets _ -> pure ()

fakeEpochs :: (Member (State Model) r) => InterpreterFor FanInEpochMembershipStore r
fakeEpochs = interpret $ \case
  GetFanInEpochsForClient _ client -> gets @Model (Map.findWithDefault [] client . epochs)
  _ -> error "Delivery unexpectedly wrote epoch membership"

fakeAcks :: (Member (State Model) r) => InterpreterFor FanInAckStore r
fakeAcks = interpret $ \case
  GetFanInAck key -> gets @Model (Map.lookup key . acks)
  HasFanInEmptyCursor value -> gets @Model (elem value . markers)
  CreateFanInEmptyCursor -> modify @Model (\m -> m {markers = nid 0 : m.markers}) >> pure (nid 0)
  InitializeFanInAck key value -> do
    modify @Model (\m -> m {acks = Map.insertWith (\_ old -> old) key value m.acks})
    gets @Model (Map.findWithDefault value key . acks)
  AdvanceFanInAck key value -> modify @Model (\m -> m {acks = Map.insertWith max key value m.acks})
  _ -> error "Delivery unexpectedly reset or deleted an acknowledgement"

fakeNotifications :: (Member (State Model) r) => InterpreterFor FanInNotificationStore r
fakeNotifications = interpret $ \case
  GetLastFanInUserNotificationId u -> lastFor (U u)
  GetLastFanInClientNotificationId u c -> lastFor (C u c)
  GetLastFanInTeamNotificationId t -> lastFor (T t)
  GetLastFanInEpochNotificationId g e -> lastFor (E g e)
  GetLastFanInLocalConnectionNotificationId u -> lastFor (L u)
  GetLastFanInRemoteConnectionNotificationId d u -> lastFor (R d u)
  HasFanInUserNotification u n -> has (U u) n
  HasFanInClientNotification u c n -> has (C u c) n
  HasFanInTeamNotification t n -> has (T t) n
  HasFanInEpochNotification g e n -> has (E g e) n
  HasFanInLocalConnectionNotification u n -> has (L u) n
  HasFanInRemoteConnectionNotification d u n -> has (R d u) n
  GetFanInUserNotificationPage u n size -> page (U u) n size
  GetFanInClientNotificationPage u c n size -> page (C u c) n size
  GetFanInTeamNotificationPage t n size -> page (T t) n size
  GetFanInEpochNotificationPage g e n size -> page (E g e) n size
  GetFanInLocalConnectionNotificationPage u n size -> page (L u) n size
  GetFanInRemoteConnectionNotificationPage d u n size -> page (R d u) n size
  _ -> error "Delivery unexpectedly appended a notification"
  where
    lastFor key = gets @Model (Map.lookup key . heads)
    has key value = gets @Model (\m -> any ((== value) . fanInNotificationId) (Map.findWithDefault [] key m.rows))
    page key cursor size = do
      notifications <- gets @Model (filter (\n -> maybe True (< n.fanInNotificationId) cursor) . Map.findWithDefault [] key . rows)
      let limit = fromIntegral (fromRange size)
      pure $ FanInNotificationPage (take limit notifications) (length notifications > limit)

addRows :: (Member (State Model) r) => Scope -> [Word32] -> Sem r ()
addRows key values = modify @Model $ \m -> m {rows = Map.insertWith (flip (<>)) key (map notification values) m.rows, heads = Map.insertWith max key (nid (maximum values)) m.heads}

initial :: Model
initial = Model (Map.singleton (U uid) [notification 1]) (Map.singleton (U uid) (nid 1)) [] Map.empty [cid] [] [] Map.empty

uid :: UserId
uid = Id UUID.nil

cid :: ClientId
cid = ClientId 1

nid :: Word32 -> NotificationId
nid n = Id (UUID.fromWords 0 0x7000 0x80000000 n)

notification :: Word32 -> FanInNotification
notification n = FanInNotification (nid n) (A.toJSON [A.object ["type" A..= ("test" :: Text)]]) Nothing

eventIds :: [FanInServerMessage] -> [NotificationId]
eventIds messages = [view queuedNotificationId event | FanInEvent (EventData event _) <- messages]
