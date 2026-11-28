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

module Test.Wire.API.Golden.Manual.FanIn (tests) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Domain (Domain (..))
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.Qualified (Qualified (..))
import Data.UUID qualified as UUID
import Imports
import Test.Tasty
import Test.Wire.API.Golden.Runner (testObjects)
import Wire.API.Event.WebSocketProtocol (AckData (..), EventData (..))
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.Notification (NotificationId, queuedNotification)
import Wire.API.Notification.FanIn
import Wire.API.Push.V2 (ApsLocKey (..), Priority (..), Route (..))
import Wire.API.Push.V2 qualified as Push

tests :: TestTree
tests =
  testGroup "FanIn" $
    concat
      [ testObjects
          [ (FanInTargetUser fixtureUser, "testObject_FanInTarget_user.json"),
            (FanInTargetUserClients fixtureUser (ClientId 1 :| [ClientId 2]), "testObject_FanInTarget_clients.json"),
            (FanInTargetTeam fixtureTeam, "testObject_FanInTarget_team.json"),
            (FanInTargetEpoch fixtureGroup (Epoch 7), "testObject_FanInTarget_epoch.json"),
            (FanInTargetConnections (Qualified fixtureUser (Domain "local.example.com")), "testObject_FanInTarget_local_connections.json"),
            (FanInTargetConnections (Qualified fixtureUser (Domain "remote.example.com")), "testObject_FanInTarget_remote_connections.json")
          ],
        testObjects
          [ (fixturePush, "testObject_FanInPush_minimal.json"),
            (fixtureTransientPush, "testObject_FanInPush_transient.json")
          ],
        testObjects [(fixtureMember, "testObject_FanInMember.json")],
        testObjects
          [ (FanInMembershipUpdate fixtureGroup (Epoch 7) [fixtureMember], "testObject_FanInMembershipUpdate_members.json"),
            (FanInMembershipUpdate fixtureGroup (Epoch 8) [], "testObject_FanInMembershipUpdate_empty.json")
          ],
        testObjects
          [ (FanInAcknowledgements Nothing Nothing Nothing [], "testObject_FanInAcknowledgements_empty.json"),
            (FanInAcknowledgements (Just fixtureNotification) (Just fixtureNotification) (Just fixtureNotification) [fixtureMLSAck, fixtureMLSAck {ackPreviousEpochs = True}], "testObject_FanInAcknowledgements_full.json")
          ],
        testObjects
          [ (fixtureMLSAck, "testObject_FanInMLSAck_current.json"),
            (fixtureMLSAck {ackPreviousEpochs = True}, "testObject_FanInMLSAck_previous.json")
          ],
        testObjects
          [ (FanInEvent (EventData (queuedNotification fixtureNotification (mempty :| [])) 123), "testObject_FanInServerMessage_event.json"),
            (FanInNotificationsMissed, "testObject_FanInServerMessage_missed.json"),
            (FanInSynchronization, "testObject_FanInServerMessage_synchronization.json")
          ],
        testObjects
          [ (FanInAck (AckData 123 False), "testObject_FanInClientMessage_ack.json"),
            (FanInAck (AckData 123 True), "testObject_FanInClientMessage_ack_multiple.json"),
            (FanInAckFullSync, "testObject_FanInClientMessage_ack_full_sync.json")
          ],
        testObjects
          [ (FanInCannonRequest (fixtureMember :| []) fixturePush, "testObject_FanInCannonRequest_persistent.json"),
            (FanInCannonRequest (fixtureMember :| [FanInMember fixtureUser (ClientId 2)]) fixtureTransientPush, "testObject_FanInCannonRequest_transient.json")
          ]
      ]

fixtureUser :: UserId
fixtureUser = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))

fixtureTeam :: TeamId
fixtureTeam = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))

fixtureNotification :: NotificationId
fixtureNotification = Id (fromJust (UUID.fromString "019ef8d3-ed95-7781-9c19-df5c8dfa4c7a"))

fixtureGroup :: GroupId
fixtureGroup = GroupId "fan-in-example-group"

fixtureMember :: FanInMember
fixtureMember = FanInMember fixtureUser (ClientId 1)

fixtureMLSAck :: FanInMLSAck
fixtureMLSAck = FanInMLSAck fixtureGroup (Epoch 7) fixtureNotification False

fixturePush :: FanInPush
fixturePush =
  FanInPush
    { notificationId = fixtureNotification,
      targets = FanInTargetUser fixtureUser :| [],
      conn = Nothing,
      transient = False,
      route = RouteAny,
      nativePriority = Nothing,
      origin = Nothing,
      json = mempty,
      apsData = Nothing,
      isCellsEvent = False
    }

fixtureTransientPush :: FanInPush
fixtureTransientPush =
  fixturePush
    { targets = FanInTargetTeam fixtureTeam :| [FanInTargetEpoch fixtureGroup (Epoch 7)],
      conn = Just (ConnId "example-connection"),
      transient = True,
      route = RouteDirect,
      nativePriority = Just HighPriority,
      origin = Just fixtureUser,
      json = KeyMap.singleton "type" (A.String "example.event"),
      apsData = Just (Push.apsData (ApsLocKey "EXAMPLE_EVENT") ["example"]),
      isCellsEvent = True
    }
