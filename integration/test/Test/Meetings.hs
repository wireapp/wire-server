{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Meetings where

import API.Galley
import qualified API.GalleyInternal as I
import Data.Time.Clock
import qualified Data.Time.Format as Time
import SetupHelpers
import Testlib.Prelude

-- Helper to extract meetingId and domain from a meeting JSON object
getMeetingIdAndDomain :: (HasCallStack) => Value -> App (String, String)
getMeetingIdAndDomain meeting = do
  meetingId <- meeting %. "qualified_id" %. "id" >>= asString
  domain <- meeting %. "qualified_id" %. "domain" >>= asString
  pure (meetingId, domain)

-- Helper to create a default new meeting JSON object
defaultMeetingJson :: String -> UTCTime -> UTCTime -> [String] -> Value
defaultMeetingJson title startTime endTime invitedEmails =
  object
    [ "title" .= title,
      "start_time" .= startTime,
      "end_time" .= endTime,
      "invited_emails" .= invitedEmails
    ]

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
  meeting %. "title" `shouldMatch` ("Team Standup" :: String)
  meeting %. "qualified_creator" %. "id" `shouldMatch` ownerId
  meeting %. "invited_emails" `shouldMatch` (["alice@example.com", "bob@example.com"] :: [String])

  -- Verify fetching the meeting
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  r2 <- getMeeting owner domain meetingId
  assertSuccess r2

  fetchedMeeting <- getJSON 200 r2
  fetchedMeeting %. "title" `shouldMatch` ("Team Standup" :: String)

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

  let firstMeeting = object ["status" .= "enabled"]
  I.setTeamFeatureLockStatus owner tid "meetingsPremium" "unlocked"
  I.setTeamFeatureConfig owner tid "meetingsPremium" firstMeeting >>= assertStatus 200

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
  let firstMeeting = object ["status" .= "disabled", "lockStatus" .= "unlocked"]
  I.setTeamFeatureConfig owner tid "meetings" firstMeeting >>= assertStatus 200

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
        object
          [ "frequency" .= "daily",
            "interval" .= (1 :: Int),
            "until" .= recurrenceUntil
          ]
      newMeeting =
        object
          [ "title" .= "Daily Standup with Recurrence",
            "start_time" .= startTime,
            "end_time" .= endTime,
            "recurrence" .= recurrence,
            "invited_emails" .= ["charlie@example.com"]
          ]

  r1 <- postMeetings owner newMeeting
  assertSuccess r1

  meeting <- getJSON 201 r1
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let updatedRecurrence =
        object
          [ "frequency" .= "weekly",
            "interval" .= (2 :: Int)
          ]
      updatedMeeting =
        object
          [ "title" .= "Updated Standup",
            "start_date" .= startTime,
            "end_date" .= endTime,
            "recurrence" .= updatedRecurrence
          ]

  r2 <- putMeeting owner domain meetingId updatedMeeting
  assertSuccess r2

  updated <- getJSON 200 r2
  updated %. "title" `shouldMatch` ("Updated Standup" :: String)
  recurrence' <- updated %. "recurrence"
  recurrence' %. "frequency" `shouldMatch` "weekly"
  recurrence' %. "interval" `shouldMatchInt` 2

testMeetingUpdateNotFound :: (HasCallStack) => App ()
testMeetingUpdateNotFound = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  fakeMeetingId <- randomId
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 3600 now
      endTime = addUTCTime 7200 now
      update =
        object
          [ "title" .= "Updated",
            "start_date" .= startTime,
            "end_date" .= endTime
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

  meeting <- getJSON 201 r1
  (meetingId, domain) <- getMeetingIdAndDomain meeting
  let update =
        object
          [ "title" .= "Hijacked",
            "start_date" .= startTime,
            "end_date" .= endTime
          ]

  putMeeting otherUser domain meetingId update >>= assertStatus 404

testMeetingListEmpty :: (HasCallStack) => App ()
testMeetingListEmpty = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  resp <- getMeetingsList owner
  assertSuccess resp
  meetings <- resp.json & asList
  length (meetings :: [Value]) `shouldMatchInt` 0

testMeetingListNoMeetings :: (HasCallStack) => App ()
testMeetingListNoMeetings = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  _ <- createTeam OwnDomain 1
  resp <- getMeetingsList owner
  assertSuccess resp
  meetings <- resp.json & asList
  length (meetings :: [Value]) `shouldMatchInt` 0

testMeetingListMultiple :: (HasCallStack) => App ()
testMeetingListMultiple = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime
  let firstMeeting = defaultMeetingJson "First Meeting" (addUTCTime 3600 now) (addUTCTime 7200 now) []
      secondMeeting = defaultMeetingJson "Second Meeting" (addUTCTime 3600 now) (addUTCTime 7200 now) []
      thirdMeeting = defaultMeetingJson "Third Meeting" (addUTCTime 3600 now) (addUTCTime 7200 now) []
  r1 <- postMeetings owner firstMeeting
  assertSuccess r1
  m1 <- getJSON 201 r1
  (id1, _) <- getMeetingIdAndDomain m1

  r2 <- postMeetings owner secondMeeting
  assertSuccess r2
  m2 <- getJSON 201 r2
  (id2, _) <- getMeetingIdAndDomain m2

  r3 <- postMeetings owner thirdMeeting
  assertSuccess r3
  m3 <- getJSON 201 r3
  (id3, _) <- getMeetingIdAndDomain m3

  resp <- getMeetingsList owner
  assertSuccess resp
  meetings <- resp.json & asList
  length (meetings :: [Value]) `shouldMatchInt` 3

  titles <- forM meetings $ \m -> m %. "title" >>= asString
  let expectedTitles = ["First Meeting", "Second Meeting", "Third Meeting"]
  (all (`elem` titles) expectedTitles) `shouldMatch` True

  fetchedIds <- forM meetings $ \m -> m %. "qualified_id" %. "id" >>= asString
  let expectedIds = [id1, id2, id3]
  (all (`elem` fetchedIds) expectedIds) `shouldMatch` True

testMeetingListPagination :: (HasCallStack) => App ()
testMeetingListPagination = do
  (owner, _tid, _members) <- createTeam OwnDomain 1
  now <- liftIO getCurrentTime

  -- The internal page size is 1000, so we create 1001 meetings to test pagination.
  -- This ensures `hasMore = True` is triggered and multiple pages are fetched.
  forM_ [(1 :: Int) .. 1001] $ \i -> do
    let meeting = defaultMeetingJson ("Meeting " <> show i) (addUTCTime 3600 now) (addUTCTime 7200 now) []
    postMeetings owner meeting >>= assertStatus 201

  resp <- getMeetingsList owner
  assertSuccess resp
  meetings <- resp.json & asList
  length (meetings :: [Value]) `shouldMatchInt` 1001
