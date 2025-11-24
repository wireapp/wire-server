-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module API.Meetings
  ( tests,
  )
where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Data.Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id (randomId, toUUID)
import Data.Qualified (qDomain, qUnqualified)
import Data.Time.Clock
import Data.UUID qualified as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit ((@?=))
import TestHelpers
import TestSetup
import Wire.API.Meeting
import Wire.API.User.Identity (emailAddressText)

-- Helper to convert MeetingId to ByteString for URL paths
meetingIdToBS :: MeetingId -> ByteString
meetingIdToBS = toByteString' . UUID.toText . unMeetingId

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Meetings API"
    [ test s "POST /meetings - create meeting" testMeetingCreate,
      test s "GET /meetings - list meetings" testMeetingLists,
      test s "GET /meetings/:domain/:id - get meeting" testMeetingGet,
      test s "GET /meetings/:domain/:id - meeting not found (404)" testMeetingGetNotFound,
      test s "PUT /meetings/:domain/:id - update meeting" testMeetingUpdate,
      test s "PUT /meetings/:domain/:id - update meeting not found (404)" testMeetingUpdateNotFound,
      test s "PUT /meetings/:domain/:id - update meeting unauthorized (404)" testMeetingUpdateUnauthorized,
      test s "DELETE /meetings/:domain/:id - delete meeting" testMeetingDelete,
      test s "DELETE /meetings/:domain/:id - delete meeting not found (404)" testMeetingDeleteNotFound,
      test s "DELETE /meetings/:domain/:id - delete meeting unauthorized (404)" testMeetingDeleteUnauthorized,
      test s "POST /meetings/:domain/:id/invitations - add invitation" testMeetingAddInvitation,
      test s "POST /meetings/:domain/:id/invitations - meeting not found (404)" testMeetingAddInvitationNotFound,
      test s "POST /meetings/:domain/:id/invitations/:email/delete - remove invitation" testMeetingRemoveInvitation,
      test s "POST /meetings/:domain/:id/invitations/:email/delete - meeting not found (404)" testMeetingRemoveInvitationNotFound,
      test s "POST /meetings - personal user creates trial meeting" testMeetingCreatePersonalUserTrial,
      test s "POST /meetings - non-paying team creates trial meeting" testMeetingCreateNonPayingTeamTrial,
      test s "POST /meetings - paying team creates non-trial meeting" testMeetingCreatePayingTeamNonTrial
    ]

testMeetingCreate :: TestM ()
testMeetingCreate = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= (["alice@example.com", "bob@example.com"] :: [Text])
          ]

  galley <- viewGalley
  r <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r :: Meeting
  liftIO $ do
    meeting.title @?= "Team Standup"
    qUnqualified meeting.creator @?= owner
    meeting.invitedEmails @?= mapMaybe emailAddressText ["alice@example.com", "bob@example.com"]

testMeetingLists :: TestM ()
testMeetingLists = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  void $
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  r <-
    get
      ( galley
          . paths ["meetings", "list"]
          . zUser owner
          . zConn "conn"
      )
      <!! const 200 === statusCode

  let meetings = responseJsonUnsafe r :: [Meeting]
  liftIO $ length meetings @?= 1

testMeetingGet :: TestM ()
testMeetingGet = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id

  r2 <-
    get
      ( galley
          . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
          . zUser owner
          . zConn "conn"
      )
      <!! const 200 === statusCode

  let fetchedMeeting = responseJsonUnsafe r2 :: Meeting
  liftIO $ fetchedMeeting.title @?= "Team Standup"

testMeetingGetNotFound :: TestM ()
testMeetingGetNotFound = do
  (owner, _tid) <- createBindingTeam
  uuid <- randomId
  let fakeMeetingId = MeetingId (toUUID uuid)
  localDomain <- viewFederationDomain

  galley <- viewGalley
  get
    ( galley
        . paths ["meetings", toByteString' localDomain, meetingIdToBS fakeMeetingId]
        . zUser owner
        . zConn "conn"
    )
    !!! const 404 === statusCode

testMeetingUpdate :: TestM ()
testMeetingUpdate = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id
      updatedMeeting =
        object
          [ "title" .= ("Updated Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime
          ]

  r2 <-
    put
      ( galley
          . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
          . zUser owner
          . zConn "conn"
          . json updatedMeeting
      )
      <!! const 200 === statusCode

  let updated = responseJsonUnsafe r2 :: Meeting
  liftIO $ updated.title @?= "Updated Standup"

testMeetingUpdateNotFound :: TestM ()
testMeetingUpdateNotFound = do
  (owner, _tid) <- createBindingTeam
  uuid <- randomId
  let fakeMeetingId = MeetingId (toUUID uuid)
  localDomain <- viewFederationDomain
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      update =
        object
          [ "title" .= ("Updated" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime
          ]

  galley <- viewGalley
  put
    ( galley
        . paths ["meetings", toByteString' localDomain, meetingIdToBS fakeMeetingId]
        . zUser owner
        . zConn "conn"
        . json update
    )
    !!! const 404 === statusCode

testMeetingUpdateUnauthorized :: TestM ()
testMeetingUpdateUnauthorized = do
  (owner, _tid) <- createBindingTeam
  (otherUser, _) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id
      update =
        object
          [ "title" .= ("Hijacked" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime
          ]

  put
    ( galley
        . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
        . zUser otherUser
        . zConn "conn"
        . json update
    )
    !!! const 404 === statusCode

testMeetingDelete :: TestM ()
testMeetingDelete = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id

  delete
    ( galley
        . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
        . zUser owner
        . zConn "conn"
    )
    !!! const 200 === statusCode

  get
    ( galley
        . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
        . zUser owner
        . zConn "conn"
    )
    !!! const 404 === statusCode

testMeetingDeleteNotFound :: TestM ()
testMeetingDeleteNotFound = do
  (owner, _tid) <- createBindingTeam
  uuid <- randomId
  let fakeMeetingId = MeetingId (toUUID uuid)
  localDomain <- viewFederationDomain

  galley <- viewGalley
  delete
    ( galley
        . paths ["meetings", toByteString' localDomain, meetingIdToBS fakeMeetingId]
        . zUser owner
        . zConn "conn"
    )
    !!! const 404 === statusCode

testMeetingDeleteUnauthorized :: TestM ()
testMeetingDeleteUnauthorized = do
  (owner, _tid) <- createBindingTeam
  (otherUser, _) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id

  delete
    ( galley
        . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
        . zUser otherUser
        . zConn "conn"
    )
    !!! const 404 === statusCode

testMeetingAddInvitation :: TestM ()
testMeetingAddInvitation = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= (["alice@example.com"] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id
      invitation = object ["emails" .= ["bob@example.com" :: Text]]

  post
    ( galley
        . paths ["meetings", toByteString' domain, meetingIdToBS meetingId, "invitations"]
        . zUser owner
        . zConn "conn"
        . json invitation
    )
    !!! const 200 === statusCode

  r2 <-
    get
      ( galley
          . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
          . zUser owner
          . zConn "conn"
      )
      <!! const 200 === statusCode

  let updated = responseJsonUnsafe r2 :: Meeting
  liftIO $ updated.invitedEmails @?= mapMaybe emailAddressText ["alice@example.com", "bob@example.com"]

testMeetingAddInvitationNotFound :: TestM ()
testMeetingAddInvitationNotFound = do
  (owner, _tid) <- createBindingTeam
  uuid <- randomId
  let fakeMeetingId = MeetingId (toUUID uuid)
  localDomain <- viewFederationDomain
  let invitation = object ["emails" .= ["bob@example.com" :: Text]]

  galley <- viewGalley
  post
    ( galley
        . paths ["meetings", toByteString' localDomain, meetingIdToBS fakeMeetingId, "invitations"]
        . zUser owner
        . zConn "conn"
        . json invitation
    )
    !!! const 404 === statusCode

testMeetingRemoveInvitation :: TestM ()
testMeetingRemoveInvitation = do
  (owner, _tid) <- createBindingTeam
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Team Standup" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= (["alice@example.com", "bob@example.com"] :: [Text])
          ]

  galley <- viewGalley
  r1 <-
    post
      ( galley
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r1 :: Meeting
      meetingId = qUnqualified meeting.id
      domain = qDomain meeting.id
      removeInvitation = object ["emails" .= ["alice@example.com" :: Text]]

  post
    ( galley
        . paths ["meetings", toByteString' domain, meetingIdToBS meetingId, "invitations", "delete"]
        . zUser owner
        . zConn "conn"
        . json removeInvitation
    )
    !!! const 200 === statusCode

  r2 <-
    get
      ( galley
          . paths ["meetings", toByteString' domain, meetingIdToBS meetingId]
          . zUser owner
          . zConn "conn"
      )
      <!! const 200 === statusCode

  let updated = responseJsonUnsafe r2 :: Meeting
  liftIO $ updated.invitedEmails @?= mapMaybe emailAddressText ["bob@example.com"]

testMeetingRemoveInvitationNotFound :: TestM ()
testMeetingRemoveInvitationNotFound = do
  (owner, _tid) <- createBindingTeam
  uuid <- randomId
  let fakeMeetingId = MeetingId (toUUID uuid)
  localDomain <- viewFederationDomain
  let removeInvitation = object ["emails" .= ["alice@example.com" :: Text]]

  galley <- viewGalley
  post
    ( galley
        . paths ["meetings", toByteString' localDomain, meetingIdToBS fakeMeetingId, "invitations", "delete"]
        . zUser owner
        . zConn "conn"
        . json removeInvitation
    )
    !!! const 404 === statusCode

-- Test that personal (non-team) users create trial meetings
testMeetingCreatePersonalUserTrial :: TestM ()
testMeetingCreatePersonalUserTrial = do
  personalUser <- randomUser
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Personal Meeting" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  galley <- viewGalley
  r <-
    post
      ( galley
          . paths ["meetings"]
          . zUser personalUser
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r :: Meeting
  liftIO $ meeting.trial @?= True

-- Test that non-paying team members create trial meetings
testMeetingCreateNonPayingTeamTrial :: TestM ()
testMeetingCreateNonPayingTeamTrial = do
  (owner, tid) <- createBindingTeam

  -- Ensure payingTeam feature is disabled (default)
  g <- viewGalley
  void $
    put
      ( g
          . paths ["i", "teams", toByteString' tid, "features", "payingTeam"]
          . json (object ["status" .= ("disabled" :: Text)])
      )
      <!! const 200 === statusCode

  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Non-Paying Team Meeting" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  r <-
    post
      ( g
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r :: Meeting
  liftIO $ meeting.trial @?= True

-- Test that paying team members create non-trial meetings
testMeetingCreatePayingTeamNonTrial :: TestM ()
testMeetingCreatePayingTeamNonTrial = do
  (owner, tid) <- createBindingTeam

  -- Enable payingTeam feature
  g <- viewGalley
  void $
    put
      ( g
          . paths ["i", "teams", toByteString' tid, "features", "payingTeam"]
          . json (object ["status" .= ("enabled" :: Text)])
      )
      <!! const 200 === statusCode

  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting =
        object
          [ "title" .= ("Paying Team Meeting" :: Text),
            "start_date" .= startTime,
            "end_date" .= endTime,
            "invited_emails" .= ([] :: [Text])
          ]

  r <-
    post
      ( g
          . paths ["meetings"]
          . zUser owner
          . zConn "conn"
          . json newMeeting
      )
      <!! const 201 === statusCode

  let meeting = responseJsonUnsafe r :: Meeting
  liftIO $ meeting.trial @?= False
