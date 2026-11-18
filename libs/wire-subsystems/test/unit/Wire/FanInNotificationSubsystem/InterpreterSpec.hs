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

module Wire.FanInNotificationSubsystem.InterpreterSpec (spec) where

import Data.Aeson qualified as A
import Data.Domain (Domain (..))
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Qualified (Qualified (..))
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Test.Hspec
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.Notification (NotificationId)
import Wire.API.Notification.FanIn
import Wire.API.Push.V2 (Route (..))
import Wire.FanInEpochMembershipStore
import Wire.FanInNotificationStore
import Wire.FanInNotificationSubsystem
import Wire.FanInNotificationSubsystem.Interpreter
import Wire.MockInterpreters.Now (defaultTime, interpretNowConst)

data Write
  = UserWrite UserId FanInNotification
  | ClientWrite (UserId, ClientId) FanInNotification
  | TeamWrite TeamId FanInNotification
  | EpochWrite (GroupId, Epoch) FanInNotification
  | LocalWrite UserId FanInNotification
  | RemoteWrite (Domain, UserId) FanInNotification
  deriving stock (Eq, Show)

spec :: Spec
spec = describe "Fan-in publishing" $ do
  it "routes all target kinds and stores the notification payload as an event array" $ do
    let teamId = Id UUID.nil
        groupId = GroupId "group"
        p = fixture {targets = FanInTargetUser uid :| [FanInTargetUserClients uid (ClientId 1 :| [ClientId 2]), FanInTargetTeam teamId, FanInTargetEpoch groupId (Epoch 7), FanInTargetConnections (Qualified uid localDomain), FanInTargetConnections (Qualified uid remoteDomain)]}
        n = FanInNotification p.notificationId (A.toJSON [A.Object p.json]) p.origin
    runPublish (Just localDomain) [p] `shouldBe` (Right (), [UserWrite uid n, ClientWrite (uid, ClientId 1) n, ClientWrite (uid, ClientId 2) n, TeamWrite teamId n, EpochWrite (groupId, Epoch 7) n, LocalWrite uid n, RemoteWrite (remoteDomain, uid) n])
  it "rejects a transient entry before writing any preceding persistent entries" $
    runPublish (Just localDomain) [fixture, fixture {transient = True}] `shouldBe` (Left FanInTransientUnsupported, [])
  it "rejects an invalid UUID before writing any preceding valid entries" $
    runPublish (Just localDomain) [fixture, fixtureWithId (Id UUID.nil)] `shouldBe` (Left FanInInvalidNotificationId, [])
  it "requires a local domain for connection targets before writing anything" $
    runPublish Nothing [fixture, fixture {targets = FanInTargetConnections (Qualified uid remoteDomain) :| []}] `shouldBe` (Left FanInLocalDomainMissing, [])
  it "does not require a local domain for user targets" $
    fst (runPublish Nothing [fixture]) `shouldBe` Right ()
  it "accepts an empty batch without store calls" $
    runPublish Nothing [] `shouldBe` (Right (), [])

runPublish :: Maybe Domain -> [FanInPush] -> (Either FanInNotificationSubsystemError (), [Write])
runPublish domain pushes = swap . run . runState ([] :: [Write]) . runError @FanInNotificationSubsystemError . interpretNowConst defaultTime . rejectMembership . recordWrites . interpretFanInNotificationSubsystem domain $ publishFanInNotifications pushes

recordWrites :: (Member (State [Write]) r) => InterpreterFor FanInNotificationStore r
recordWrites = interpret $ \case
  AppendFanInUserNotifications rows -> modify (<> [UserWrite k n | (k, n) <- NE.toList rows])
  AppendFanInClientNotifications rows -> modify (<> [ClientWrite k n | (k, n) <- NE.toList rows])
  AppendFanInTeamNotifications rows -> modify (<> [TeamWrite k n | (k, n) <- NE.toList rows])
  AppendFanInEpochNotifications rows -> modify (<> [EpochWrite k n | (k, n) <- NE.toList rows])
  AppendFanInLocalConnectionNotifications rows -> modify (<> [LocalWrite k n | (k, n) <- NE.toList rows])
  AppendFanInRemoteConnectionNotifications rows -> modify (<> [RemoteWrite k n | (k, n) <- NE.toList rows])
  _ -> error "Publishing unexpectedly read notification storage"

uid :: UserId
uid = Id UUID.nil

localDomain, remoteDomain :: Domain
localDomain = Domain "local.example.com"
remoteDomain = Domain "remote.example.com"

fixture :: FanInPush
fixture = fixtureWithId (Id (fromJust (UUID.fromString "017f22e2-79b0-7cc3-98c4-dc0c0c07398f")))

fixtureWithId :: NotificationId -> FanInPush
fixtureWithId nid =
  FanInPush
    { notificationId = nid,
      targets = FanInTargetUser uid :| [],
      conn = Nothing,
      transient = False,
      route = RouteAny,
      nativePriority = Nothing,
      origin = Just uid,
      json = mempty,
      apsData = Nothing,
      isCellsEvent = False
    }

rejectMembership :: InterpreterFor FanInEpochMembershipStore r
rejectMembership = interpret $ \_ -> error "Publishing unexpectedly accessed epoch membership"
