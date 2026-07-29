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

module Wire.MeetingNotifierSpec (spec) where

import Control.Concurrent.Async qualified as A
import Data.Aeson (Result (..), Value (Object), fromJSON)
import Data.Domain (Domain (..))
import Data.Id
import Data.Qualified (Qualified (..))
import Data.Range (unsafeRange)
import Data.Set qualified as Set
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Imports
import Polysemy
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import Test.Hspec
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.MeetingNotifier
import Wire.MeetingNotifier.Interpreter
import Wire.MeetingsStore qualified as Store
import Wire.MockInterpreters.Now (interpretNowConst)
import Wire.NotificationSubsystem
import Wire.Sem.Logger.TinyLog (discardTinyLogs)
import Wire.Sem.Now (Now)

spec :: Spec
spec = do
  describe "newLocalMeetingMembers" $ do
    it "returns only users absent from the previous local membership set" $ do
      let existingUser = Id $ read "00000000-0000-0000-0000-000000000001"
          newLocalUser = Id $ read "00000000-0000-0000-0000-000000000002"
          beforeSet = Set.singleton existingUser
          afterSet =
            Set.fromList
              [ existingUser,
                newLocalUser
              ]

      newLocalMeetingMembers beforeSet afterSet `shouldBe` [newLocalUser]

    it "returns no users when only client membership changes" $ do
      let existingUser = Id $ read "00000000-0000-0000-0000-000000000001"
          members = Set.singleton existingUser

      newLocalMeetingMembers members members `shouldBe` []

  describe "interpretMeetingNotifier" $ do
    it "pushes a member-add event for each alive meeting" $ do
      let now = UTCTime (ModifiedJulianDay 60000) 0
          actor = Id $ read "00000000-0000-0000-0000-000000000001"
          addedUser = Id $ read "00000000-0000-0000-0000-000000000002"
          convId = Id $ read "00000000-0000-0000-0000-000000000003"
          meetingId = Id $ read "00000000-0000-0000-0000-000000000004"
          domain = Domain "local.example.com"
          meeting = storedMeeting meetingId convId now (addUTCTime 60 now)

      pushes <-
        runMemberAdded now [meeting] $
          notifyMeetingMembersAdded
            (Qualified actor domain)
            (Qualified convId domain)
            Nothing
            [addedUser]

      length pushes `shouldBe` 1
      let push = head pushes
      push.recipients `shouldBe` [userRecipient addedUser]
      case fromJSON (Object push.json) :: Result MeetingEvent.Event of
        Error err -> expectationFailure err
        Success event -> do
          event.evtType `shouldBe` MeetingEvent.MemberAdd
          event.evtMeeting `shouldBe` Qualified meetingId domain
          event.evtConv `shouldBe` Qualified convId domain

    it "does not push when no alive meeting exists" $ do
      let now = UTCTime (ModifiedJulianDay 60000) 0
          actor = Id $ read "00000000-0000-0000-0000-000000000001"
          addedUser = Id $ read "00000000-0000-0000-0000-000000000002"
          convId = Id $ read "00000000-0000-0000-0000-000000000003"
          domain = Domain "local.example.com"

      pushes <-
        runMemberAdded now [] $
          notifyMeetingMembersAdded
            (Qualified actor domain)
            (Qualified convId domain)
            Nothing
            [addedUser]

      pushes `shouldBe` []

runMemberAdded ::
  UTCTime ->
  [Store.StoredMeeting] ->
  Sem
    '[ MeetingNotifier,
       Store.MeetingsStore,
       NotificationSubsystem,
       Now,
       TinyLog,
       State [Push],
       Embed IO
     ]
    () ->
  IO [Push]
runMemberAdded now meetings =
  runM
    . execState ([] :: [Push])
    . discardTinyLogs
    . interpretNowConst now
    . captureNotifications
    . interpretMeetingsStore meetings
    . interpretMeetingNotifier

captureNotifications ::
  (Member (State [Push]) r, Member (Embed IO) r) =>
  InterpreterFor NotificationSubsystem r
captureNotifications = interpret $ \case
  PushNotificationAsync push ->
    modify (<> [push]) >> embed (A.async (pure (Just ())))
  _ -> error "unexpected notification operation"

interpretMeetingsStore ::
  [Store.StoredMeeting] ->
  InterpreterFor Store.MeetingsStore r
interpretMeetingsStore meetings = interpret $ \case
  Store.ListMeetingsByConversation _ _ -> pure meetings
  _ -> error "unexpected meetings store operation"

storedMeeting ::
  MeetingId ->
  ConvId ->
  UTCTime ->
  UTCTime ->
  Store.StoredMeeting
storedMeeting meetingId convId startTime endTime =
  Store.StoredMeeting
    { Store.id = meetingId,
      Store.title = unsafeRange "Meeting",
      Store.creator = Id $ read "00000000-0000-0000-0000-000000000001",
      Store.startTime = startTime,
      Store.endTime = endTime,
      Store.recurrence = Nothing,
      Store.conversationId = convId,
      Store.invitedEmails = [],
      Store.trial = False,
      Store.createdAt = startTime,
      Store.updatedAt = startTime
    }
