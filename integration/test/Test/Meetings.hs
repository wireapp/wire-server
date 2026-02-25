{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Meetings where

import API.Galley
import qualified API.GalleyInternal as I
import Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Time.Clock
import qualified Data.Time.Format as Time
import SetupHelpers
import Testlib.Prelude as P hiding ((.=))

-- Helper to extract meetingId and domain from a meeting JSON object
getMeetingIdAndDomain :: (HasCallStack) => Value -> App (String, String)
getMeetingIdAndDomain meeting = do
  meetingId <- meeting %. "qualified_id" %. "id" >>= asString
  domain <- meeting %. "qualified_id" %. "domain" >>= asString
  pure (meetingId, domain)

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

  meeting <- getJSON 201 resp
  meeting %. "title" `shouldMatch` "Team Standup"
  meeting %. "qualified_creator" %. "id" `shouldMatch` ownerId
  meeting %. "invited_emails" `shouldMatch` ["alice@example.com", "bob@example.com"]

  -- Verify fetching the meeting
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  fetchedMeeting <- getJSON 200 r2
  fetchedMeeting %. "title" `shouldMatch` "Team Standup"

testMeetingGetNotFound :: (HasCallStack) => App ()
testMeetingGetNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId

  getMeeting owner "example.com" fakeMeetingId >>= assertLabel 404 "meeting-not-found"

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

  meeting <- getJSON 201 r
  meeting %. "trial" `shouldMatch` True

-- Test that paying team members create non-trial meetings
testMeetingCreatePayingTeamNonTrial :: (HasCallStack) => App ()
testMeetingCreatePayingTeamNonTrial = do
  (owner, tid, _members) <- createTeam OwnDomain 1

  let firstMeeting = Aeson.object [Key.fromString "status" .= Key.fromString "enabled"]
  I.setTeamFeatureLockStatus owner tid "meetingsPremium" "unlocked"
  setTeamFeatureConfig owner tid "meetingsPremium" firstMeeting >>= assertStatus 200

  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Paying Team Meeting" startTime endTime []

  r <- postMeetings owner newMeeting
  assertSuccess r

  meeting <- getJSON 201 r
  meeting %. "trial" `shouldMatch` False

-- Test that disabled MeetingsConfig feature blocks creation
testMeetingsConfigDisabledBlocksCreate :: (HasCallStack) => App ()
testMeetingsConfigDisabledBlocksCreate = do
  (owner, tid, _members) <- createTeam OwnDomain 1

  -- Disable the MeetingsConfig feature
  let firstMeeting = Aeson.object [Key.fromString "status" .= Key.fromString "disabled", Key.fromString "lockStatus" .= Key.fromString "unlocked"]
  setTeamFeatureConfig owner tid "meetings" firstMeeting >>= assertStatus 200

  -- Try to create a meeting - should fail
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      newMeeting = defaultMeetingJson "Team Standup" startTime endTime []

  postMeetings owner newMeeting >>= assertLabel 403 "invalid-op"

testMeetingRecurrence :: (HasCallStack) => App ()
testMeetingRecurrence = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      recurrenceUntil = Time.formatTime Time.defaultTimeLocale "%FT%TZ" $ addUTCTime (30 * nominalDay) now -- format to avoid rounding expectation mismatch
      recurrence =
        Aeson.object
          [ Key.fromString "frequency" .= Key.fromString "daily",
            Key.fromString "interval" .= (1 :: Int),
            Key.fromString "until" .= recurrenceUntil
          ]
      newMeeting =
        Aeson.object
          [ Key.fromString "title" .= Key.fromString "Daily Standup with Recurrence",
            Key.fromString "start_time" .= startTime,
            Key.fromString "end_time" .= endTime,
            Key.fromString "recurrence" .= recurrence,
            Key.fromString "invited_emails" .= ["charlie@example.com"]
          ]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- getJSON 201 r1
  (meetingId, domain) <- getMeetingIdAndDomain meeting

  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  fetchedMeeting <- getJSON 200 r2
  fetchedMeeting %. "title" `shouldMatch` "Daily Standup with Recurrence"
  recurrence' <- fetchedMeeting %. "recurrence"
  recurrence' %. "frequency" `shouldMatch` "daily"
  recurrence' %. "interval" `shouldMatchInt` 1
  recurrence' %. "until" `shouldMatch` recurrenceUntil

testMeetingCreateInvalidTimes :: (HasCallStack) => App ()
testMeetingCreateInvalidTimes = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTimeInvalid = addUTCTime 3500 now -- endTime is before startTime
      newMeetingInvalid = defaultMeetingJson "Invalid Time" startTime endTimeInvalid []

  postMeetings owner newMeetingInvalid >>= assertLabel 403 "invalid-op"

-- Helper to create a default new meeting JSON object
defaultMeetingJson :: String -> UTCTime -> UTCTime -> [String] -> Value
defaultMeetingJson title startTime endTime invitedEmails =
  Aeson.object
    [ Key.fromString "title" .= title,
      Key.fromString "start_time" .= startTime,
      Key.fromString "end_time" .= endTime,
      Key.fromString "invited_emails" .= invitedEmails
    ]
