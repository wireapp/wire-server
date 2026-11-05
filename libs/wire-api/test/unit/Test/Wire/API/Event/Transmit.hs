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

module Test.Wire.API.Event.Transmit (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Event.Conversation qualified as Conv
import Wire.API.Event.Meeting qualified as Mtg
import Wire.API.Event.Transmit
import Wire.API.Internal.Notification qualified as Internal
import Wire.API.Notification
import Wire.API.Routes.Version

tests :: TestTree
tests =
  testGroup
    "Transmit"
    [ gatedTypesTests,
      passthroughTests,
      malformedTests,
      notificationTests,
      driftGuardTests
    ]

typedEvent :: Text -> Event
typedEvent t = KeyMap.singleton "type" (Aeson.String t)

gatedTypes :: [Text]
gatedTypes =
  [ "conversation.create-meeting",
    "conversation.delete-meeting",
    "meeting.create",
    "meeting.update",
    "meeting.delete",
    "meeting.member-add"
  ]

passthroughTypes :: [Text]
passthroughTypes = ["conversation.create", "user.update", "team.member-join"]

gatedTypesTests :: TestTree
gatedTypesTests =
  testGroup
    "gated types are dropped below V15 and delivered at V15"
    ( [ testCase ("dropped at V14: " <> T.unpack t) $
          transmitEvent V14 (typedEvent t) @?= Nothing
      | t <- gatedTypes
      ]
        ++ [ testCase ("delivered at V15: " <> T.unpack t) $
               transmitEvent V15 (typedEvent t) @?= Just (typedEvent t)
           | t <- gatedTypes
           ]
    )

passthroughTests :: TestTree
passthroughTests =
  testGroup
    "non-gated types and objects without a type key are always delivered"
    ( [ testCase ("delivered at V14: " <> T.unpack t) $
          transmitEvent V14 (typedEvent t) @?= Just (typedEvent t)
      | t <- passthroughTypes
      ]
        ++ [ testCase "no type key delivered at V14" $
               transmitEvent V14 (KeyMap.fromList []) @?= Just (KeyMap.fromList []),
             testCase "no type key delivered at V15" $
               transmitEvent V15 (KeyMap.fromList []) @?= Just (KeyMap.fromList []),
             testCase "non-string type delivered at V14" $
               transmitEvent V14 (KeyMap.singleton "type" (Aeson.Number 42)) @?= Just (KeyMap.singleton "type" (Aeson.Number 42))
           ]
    )

malformedTests :: TestTree
malformedTests =
  testGroup
    "gated type that fails to decode is dropped below its gate, passed at/above it"
    [ testCase "malformed meeting.create at V14" $
        transmitEvent V14 (typedEvent "meeting.create") @?= Nothing,
      testCase "malformed meeting.create at V15" $
        transmitEvent V15 (typedEvent "meeting.create") @?= Just (typedEvent "meeting.create")
    ]

dummyId :: Id a
dummyId = Id (fromJust (UUID.fromString "7c2dc4e0-1bd0-11e4-8c21-0800200c9a66"))

notificationTests :: TestTree
notificationTests =
  testGroup
    "transmitQueuedNotification / transmitInternalNotification"
    [ testCase "all-gated payload becomes Nothing" $ do
        let qn = queuedNotification dummyId (typedEvent "meeting.create" :| [typedEvent "conversation.create-meeting"])
        transmitQueuedNotification V14 qn @?= Nothing,
      testCase "mixed payload keeps ungated events" $ do
        let keep = typedEvent "conversation.create"
            qn = queuedNotification dummyId (typedEvent "meeting.delete" :| [keep])
        transmitQueuedNotification V14 qn @?= Just (queuedNotification dummyId (keep :| [])),
      testCase "internal all-gated payload becomes Nothing" $ do
        let n = Internal.Notification dummyId False (typedEvent "meeting.update" :| [])
        transmitInternalNotification V14 n @?= Nothing,
      testCase "internal mixed payload keeps ungated events" $ do
        let keep = typedEvent "user.update"
            n = Internal.Notification dummyId False (typedEvent "meeting.update" :| [keep])
        transmitInternalNotification V14 n @?= Just (Internal.Notification dummyId False (keep :| []))
    ]

driftGuardTests :: TestTree
driftGuardTests =
  testGroup
    "dispatcher type strings match the family event type encodings"
    [ testCase "conversation meeting event types encode to the gated strings" $ do
        Aeson.toJSON Conv.ConvCreateMeeting @?= Aeson.String "conversation.create-meeting"
        Aeson.toJSON Conv.ConvDeleteMeeting @?= Aeson.String "conversation.delete-meeting",
      testCase "meeting event types encode to the gated strings" $
        Set.fromList (Aeson.toJSON @Mtg.EventType <$> [minBound .. maxBound])
          @?= Set.fromList (Aeson.String <$> ["meeting.create", "meeting.update", "meeting.delete", "meeting.member-add"]),
      testCase "no other conversation event type is gated" $
        Set.fromList
          [ Aeson.toJSON t
          | t <- [minBound .. maxBound] :: [Conv.EventType],
            t `notElem` [Conv.ConvCreateMeeting, Conv.ConvDeleteMeeting]
          ]
          `disjointFrom` gatedStrings,
      testCase "meeting event types are exactly the gated meeting strings" $
        Set.fromList (Aeson.toJSON @Mtg.EventType <$> [minBound .. maxBound])
          `disjointFrom` conversationMeetingStrings
    ]
  where
    gatedStrings :: Set Aeson.Value
    gatedStrings =
      Set.map Aeson.String (Set.fromList gatedTypes)

    conversationMeetingStrings :: Set Aeson.Value
    conversationMeetingStrings =
      Set.map Aeson.String (Set.fromList ["conversation.create-meeting", "conversation.delete-meeting"])

    disjointFrom :: Set Aeson.Value -> Set Aeson.Value -> Assertion
    disjointFrom a b = True @?= Set.null (Set.intersection a b)
