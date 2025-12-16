{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Meetings where

import Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time.Clock
import SetupHelpers
import Testlib.Prelude as P

-- Helper to extract meetingId and domain from a meeting JSON object
getMeetingIdAndDomain :: (HasCallStack) => Aeson.Value -> App (String, String)
getMeetingIdAndDomain meeting = do
  meetingId <- meeting %. "qualified_id" %. "id" >>= asString
  domain <- meeting %. "qualified_id" %. "domain" >>= asString
  return (meetingId, domain)

testMeetingCreate :: (HasCallStack) => App ()
testMeetingCreate = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  ownerId <- owner %. "id" >>= asString
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime ["alice@example.com", "bob@example.com"]

  resp <- postMeetings owner newMeeting
  assertSuccess resp

  meeting <- assertOne resp.jsonBody
  meeting %. "title" `shouldMatchText` "Team Standup"
  meeting %. "qualified_creator" %. "id" `shouldMatch` ownerId
  meeting %. "invited_emails" `shouldMatch` ["alice@example.com" :: Text, "bob@example.com"]

testMeetingLists :: (HasCallStack) => App ()
testMeetingLists = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  postMeetings owner newMeeting >>= assertStatus 201

  resp <- getMeetingsList owner
  assertSuccess resp

  meetings <- resp.jsonBody & asList
  length (meetings :: [Value]) `shouldMatchInt` 1

testMeetingGet :: (HasCallStack) => App ()
testMeetingGet = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting

  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  fetchedMeeting <- assertOne r2.jsonBody
  fetchedMeeting %. "title" `shouldMatchText` "Team Standup"

testMeetingGetNotFound :: (HasCallStack) => App ()
testMeetingGetNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId

  getMeeting owner "example.com" fakeMeetingId >>= assertStatus 404

testMeetingUpdate :: (HasCallStack) => App ()
testMeetingUpdate = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      recurrenceUntil = addUTCTime (60 * 24 * 3600) now -- 60 days from now
      initialRecurrence =
        Aeson.object
          [ "frequency" Aeson..= ("daily" :: Text),
            "interval" Aeson..= (1 :: Int),
            "until" Aeson..= recurrenceUntil
          ]
      newMeeting =
        Aeson.object
          [ "title" Aeson..= ("Team Standup" :: Text),
            "start_date" Aeson..= startTime,
            "end_date" Aeson..= endTime,
            "invited_emails" Aeson..= ([] :: [Text]),
            "recurrence" Aeson..= initialRecurrence
          ]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let updatedRecurrence =
        Aeson.object
          [ "frequency" Aeson..= ("weekly" :: Text),
            "interval" Aeson..= (2 :: Int)
          ]
      updatedMeeting =
        Aeson.object
          [ "title" Aeson..= ("Updated Standup" :: Text),
            "start_date" Aeson..= startTime,
            "end_date" Aeson..= endTime,
            "recurrence" Aeson..= updatedRecurrence
          ]

  r2 <- putMeeting owner domain meetingId updatedMeeting
  assertSuccess r2

  updated <- assertOne r2.jsonBody
  updated %. "title" `shouldMatchText` "Updated Standup"
  recurrence <- updated %. "recurrence"
  recurrence %. "frequency" `shouldMatchText` "weekly"
  recurrence %. "interval" `shouldMatchInt` 2

testMeetingUpdateNotFound :: (HasCallStack) => App ()
testMeetingUpdateNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      update =
        Aeson.object
          [ "title" Aeson..= ("Updated" :: Text),
            "start_date" Aeson..= startTime,
            "end_date" Aeson..= endTime
          ]

  putMeeting owner "example.com" fakeMeetingId update >>= assertStatus 404

testMeetingUpdateUnauthorized :: (HasCallStack) => App ()
testMeetingUpdateUnauthorized = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  (otherUser, _, _membersOther) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let update =
        Aeson.object
          [ "title" Aeson..= ("Hijacked" :: Text),
            "start_date" Aeson..= startTime,
            "end_date" Aeson..= endTime
          ]

  putMeeting otherUser domain meetingId update >>= assertStatus 404

testMeetingDelete :: (HasCallStack) => App ()
testMeetingDelete = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      recurrenceUntil = addUTCTime (30 * 24 * 3600) now
      recurrence =
        Aeson.object
          [ "frequency" Aeson..= ("daily" :: Text),
            "interval" Aeson..= (1 :: Int),
            "until" Aeson..= recurrenceUntil
          ]
      newMeeting =
        Aeson.object
          [ "title" Aeson..= ("Team Standup" :: Text),
            "start_date" Aeson..= startTime,
            "end_date" Aeson..= endTime,
            "invited_emails" Aeson..= ([] :: [Text]),
            "recurrence" Aeson..= recurrence
          ]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting

  deleteMeeting owner domain meetingId >>= assertStatus 200

  getMeeting owner domain meetingId >>= assertStatus 404

testMeetingDeleteNotFound :: (HasCallStack) => App ()
testMeetingDeleteNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId

  deleteMeeting owner "example.com" fakeMeetingId >>= assertStatus 404

testMeetingDeleteUnauthorized :: (HasCallStack) => App ()
testMeetingDeleteUnauthorized = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  (otherUser, _, _membersOther) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting

  deleteMeeting otherUser domain meetingId >>= assertStatus 404

testMeetingAddInvitation :: (HasCallStack) => App ()
testMeetingAddInvitation = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime ["alice@example.com"]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let invitation = Aeson.object ["emails" Aeson..= ["bob@example.com" :: Text]]

  postMeetingInvitation owner domain meetingId invitation >>= assertStatus 200

  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  updated <- assertOne r2.jsonBody
  updated %. "invited_emails" `shouldMatch` ["alice@example.com" :: Text, "bob@example.com"]

testMeetingAddInvitationNotFound :: (HasCallStack) => App ()
testMeetingAddInvitationNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId
  let invitation = Aeson.object ["emails" Aeson..= ["bob@example.com" :: Text]]

  postMeetingInvitation owner "example.com" fakeMeetingId invitation >>= assertStatus 404

testMeetingRemoveInvitation :: (HasCallStack) => App ()
testMeetingRemoveInvitation = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime ["alice@example.com", "bob@example.com"]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let removeInvitation = Aeson.object ["emails" Aeson..= ["alice@example.com" :: Text]]

  deleteMeetingInvitation owner domain meetingId removeInvitation >>= assertStatus 200

  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  updated <- assertOne r2.jsonBody
  updated %. "invited_emails" `shouldMatch` ["bob@example.com" :: Text]

testMeetingRemoveInvitationNotFound :: (HasCallStack) => App ()
testMeetingRemoveInvitationNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId
  let removeInvitation = Aeson.object ["emails" Aeson..= ["alice@example.com" :: Text]]

  deleteMeetingInvitation owner "example.com" fakeMeetingId removeInvitation >>= assertStatus 404

-- Test that personal (non-team) users create trial meetings
testMeetingCreatePersonalUserTrial :: (HasCallStack) => App ()
testMeetingCreatePersonalUserTrial = do
  personalUser <- randomUser OwnDomain def
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Personal Meeting" startTime endTime []

  r <- postMeetings personalUser newMeeting
  assertSuccess r

  meeting <- assertOne r.jsonBody
  meeting %. "trial" `shouldMatch` True

-- Test that non-paying team members create trial meetings
testMeetingCreateNonPayingTeamTrial :: (HasCallStack) => App ()
testMeetingCreateNonPayingTeamTrial = do
  (owner, tid, _members) <- createTeam OwnDomain 1

  let teamId = tid
  putTeamFeature owner teamId "meetingPremium" (Aeson.object ["status" Aeson..= ("disabled" :: Text)]) >>= assertStatus 200

  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Non-Paying Team Meeting" startTime endTime []

  r <- postMeetings owner newMeeting
  assertSuccess r

  meeting <- assertOne r.jsonBody
  meeting %. "trial" `shouldMatch` True

-- Test that paying team members create non-trial meetings
testMeetingCreatePayingTeamNonTrial :: (HasCallStack) => App ()
testMeetingCreatePayingTeamNonTrial = do
  (owner, tid, _members) <- createTeam OwnDomain 1

  let firstMeeting = Aeson.object ["status" Aeson..= ("enabled" :: Text)]
  putTeamFeature owner tid "meetingPremium" firstMeeting >>= assertStatus 200

  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Paying Team Meeting" startTime endTime []

  r <- postMeetings owner newMeeting
  assertSuccess r

  meeting <- assertOne r.jsonBody
  meeting %. "trial" `shouldMatch` False

-- Test that disabled MeetingConfig feature blocks creation
testMeetingConfigDisabledBlocksCreate :: (HasCallStack) => App ()
testMeetingConfigDisabledBlocksCreate = do
  (owner, tid, _members) <- createTeam OwnDomain 1

  -- Disable the MeetingConfig feature
  let firstMeeting = Aeson.object ["status" Aeson..= ("disabled" :: Text), "lockStatus" Aeson..= ("unlocked" :: Text)]
  putTeamFeature owner tid "meeting" firstMeeting >>= assertStatus 200

  -- Try to create a meeting - should fail
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  postMeetings owner newMeeting >>= assertStatus 403

-- Test that disabled MeetingConfig feature blocks meeting listing
testMeetingConfigDisabledBlocksList :: (HasCallStack) => App ()
testMeetingConfigDisabledBlocksList = do
  (owner, tid, _members) <- createTeam OwnDomain 1

  -- First create a meeting while feature is enabled
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  postMeetings owner newMeeting >>= assertStatus 201

  -- Disable the MeetingConfig feature
  let updatedMeeting = Aeson.object ["status" Aeson..= ("disabled" :: Text), "lockStatus" Aeson..= ("unlocked" :: Text)]
  putTeamFeature owner tid "meeting" updatedMeeting >>= assertStatus 200

  -- Try to list meetings - should fail
  getMeetingsList owner >>= assertStatus 403

testMeetingRecurrence :: (HasCallStack) => App ()
testMeetingRecurrence = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      recurrenceUntil = addUTCTime (30 * 24 * 3600) now -- 30 days from now
      recurrence =
        Aeson.object
          [ "frequency" Aeson..= ("daily" :: Text),
            "interval" Aeson..= (1 :: Int),
            "until" Aeson..= recurrenceUntil
          ]
      newMeeting =
        Aeson.object
          [ "title" Aeson..= ("Daily Standup with Recurrence" :: Text),
            "start_date" Aeson..= startTime,
            "end_date" Aeson..= endTime,
            "recurrence" Aeson..= recurrence,
            "invited_emails" Aeson..= ["charlie@example.com" :: Text]
          ]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting

  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  fetchedMeeting <- assertOne r2.jsonBody
  fetchedMeeting %. "title" `shouldMatchText` "Daily Standup with Recurrence"
  recurrence' <- fetchedMeeting %. "recurrence"
  recurrence' %. "frequency" `shouldMatchText` "daily"
  recurrence' %. "interval" `shouldMatchInt` 1
  recurrence' %. "until" `shouldMatch` recurrenceUntil

testMeetingCreateInvalidDates :: (HasCallStack) => App ()
testMeetingCreateInvalidDates = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTimeInvalid = addUTCTime 3500 now -- endDate is before startDate
      newMeetingInvalid = defaultMeetingJson "Invalid Date" startTime endTimeInvalid []

  postMeetings owner newMeetingInvalid >>= assertStatus 403

testMeetingUpdateInvalidDates :: (HasCallStack) => App ()
testMeetingUpdateInvalidDates = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Valid Meeting" startTime endTime []

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let updatedStartTime = addUTCTime 1800 now
      updatedEndTimeInvalid = addUTCTime 1000 now -- endDate is before startDate
      updatedMeeting =
        Aeson.object
          [ "start_date" Aeson..= updatedStartTime,
            "end_date" Aeson..= updatedEndTimeInvalid
          ]

  putMeeting owner domain meetingId updatedMeeting >>= assertStatus 403

testMeetingUpdateInvalidDatesPartialEnd :: (HasCallStack) => App ()
testMeetingUpdateInvalidDatesPartialEnd = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Valid Meeting" startTime endTime []

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let updatedEndTimeInvalid = addUTCTime 1000 now -- endDate is before startDate
      updatedMeeting =
        Aeson.object
          [ "end_date" Aeson..= updatedEndTimeInvalid
          ]

  putMeeting owner domain meetingId updatedMeeting >>= assertStatus 403

testMeetingUpdateInvalidDatesPartialStart :: (HasCallStack) => App ()
testMeetingUpdateInvalidDatesPartialStart = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Valid Meeting" startTime endTime []

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- assertOne r1.jsonBody
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let updatedStartTimeInvalid = addUTCTime 8000 now -- startDate is after endDate
      updatedMeeting =
        Aeson.object
          [ "start_date" Aeson..= updatedStartTimeInvalid
          ]

  putMeeting owner domain meetingId updatedMeeting >>= assertStatus 403

-- * Helpers

-- Custom API helper functions for meetings
postMeetings :: (HasCallStack, MakesValue user) => user -> Aeson.Value -> App Response
postMeetings user newMeeting = do
  req <- baseRequest user Galley Versioned "/meetings"
  submit "POST" $ req & addJSON newMeeting

getMeetingsList :: (HasCallStack, MakesValue user) => user -> App Response
getMeetingsList user = do
  req <- baseRequest user Galley Versioned "/meetings/list"
  submit "GET" req

getMeeting :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response
getMeeting user domain meetingId = do
  req <- baseRequest user Galley Versioned (joinHttpPath ["meetings", domain, meetingId])
  submit "GET" req

putMeeting :: (HasCallStack, MakesValue user) => user -> String -> String -> Aeson.Value -> App Response
putMeeting user domain meetingId updatedMeeting = do
  req <- baseRequest user Galley Versioned (joinHttpPath ["meetings", domain, meetingId])
  submit "PUT" $ req & addJSON updatedMeeting

deleteMeeting :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response
deleteMeeting user domain meetingId = do
  req <- baseRequest user Galley Versioned (joinHttpPath ["meetings", domain, meetingId])
  submit "DELETE" req

postMeetingInvitation :: (HasCallStack, MakesValue user) => user -> String -> String -> Aeson.Value -> App Response
postMeetingInvitation user domain meetingId invitation = do
  req <- baseRequest user Galley Versioned (joinHttpPath ["meetings", domain, meetingId, "invitations"])
  submit "POST" $ req & addJSON invitation

deleteMeetingInvitation :: (HasCallStack, MakesValue user) => user -> String -> String -> Aeson.Value -> App Response
deleteMeetingInvitation user domain meetingId removeInvitation = do
  req <- baseRequest user Galley Versioned (joinHttpPath ["meetings", domain, meetingId, "invitations", "delete"])
  submit "POST" $ req & addJSON removeInvitation

putTeamFeature :: (HasCallStack, MakesValue user) => user -> String -> String -> Aeson.Value -> App Response
putTeamFeature user tid featureName payload = do
  req <- baseRequest user Galley Unversioned (joinHttpPath ["i", "teams", tid, "features", featureName])
  submit "PUT" $ req & addJSON payload

-- Helper to create a default new meeting JSON object
defaultMeetingJson :: Text -> UTCTime -> UTCTime -> [Text] -> Aeson.Value
defaultMeetingJson title startTime endTime invitedEmails =
  Aeson.object
    [ "title" Aeson..= title,
      "start_date" Aeson..= startTime,
      "end_date" Aeson..= endTime,
      "invited_emails" Aeson..= invitedEmails
    ]
