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

module Wire.MeetingsSubsystem.InterpreterSpec (spec) where

import Data.Aeson (Result (..), Value (Object), fromJSON)
import Data.ByteString.Char8 qualified as C
import Data.Default (def)
import Data.Domain (Domain (..))
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Qualified
import Data.Range (checked, unsafeRange)
import Data.Set qualified as Set
import Data.Tagged (Tagged)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Data.UUID (nil)
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative, counterexample, getNonNegative, ioProperty, (.&&.), (===), (==>))
import Text.Email.Parser (unsafeEmailAddress)
import Wire.API.Conversation (Access (InviteAccess, PrivateAccess), Conversation (metadata, qualifiedId), ConversationMetadata (cnvmAccess))
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley (GalleyError (TeamMemberNotFound, TeamNotFound), InvalidTimesReason (..), MeetingError (..))
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.API.Meeting qualified as API
import Wire.API.PostgresMarshall (PostgresUnmarshall (postgresUnmarshall))
import Wire.API.Team.Feature
import Wire.API.Team.Member (TeamMember, mkTeamMember)
import Wire.API.Team.Permission (fullPermissions)
import Wire.ConversationSubsystem
import Wire.FeaturesConfigSubsystem
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.MeetingNotifier (MeetingNotifier, notifyMeetingEvent)
import Wire.MeetingNotifier.Interpreter (interpretMeetingNotifier)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.MeetingsSubsystem.Interpreter
import Wire.MockInterpreters
import Wire.NotificationSubsystem (NotificationSubsystem, Push (..), Recipient (recipientUserId))
import Wire.Sem.Logger.TinyLog (discardTinyLogs)
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem.GalleyAPI

type TestStack =
  '[ MeetingsSubsystem,
     MeetingNotifier,
     Store.MeetingsStore,
     ConversationSubsystem,
     TeamSubsystem,
     FeaturesConfigSubsystem,
     NotificationSubsystem,
     TinyLog,
     Error MeetingError,
     State (Map MeetingId Store.StoredMeeting),
     State (Map ConvId StoredConversation),
     State (Map ConvId (Set UserId)),
     State [Push],
     GalleyAPIAccess,
     Now,
     State UTCTime,
     Input (Local ()),
     Random,
     State StdGen,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'TeamNotFound,
     Embed IO
   ]

interpretFeaturesConfigSubsystemPure :: AllTeamFeatures -> InterpreterFor FeaturesConfigSubsystem r
interpretFeaturesConfigSubsystemPure configs = interpret $ \case
  GetDbFeatureRawInternal _tid -> pure def
  GetFeature _uid _tid -> pure def
  GetFeatureForTeam _tid -> pure $ npProject configs
  GetFeatureForServer -> pure def
  GetFeatureForTeamUser _uid _mTid -> pure def
  GetAllTeamFeaturesForTeamMember _luid _tid -> pure def
  GetAllTeamFeaturesForTeam _tid -> pure def
  GetAllTeamFeaturesForServer -> pure def
  GuardSecondFactorDisabled _ _ -> error "not implemented"
  FeatureEnabledForTeam _ _ -> error "not implemented"
  GetAllTeamFeaturesForUser _ -> error "not implemented"
  GetSingleFeatureForUser _ -> error "not implemented"
  GetFeatureInternal _ -> error "not implemented"

runTestStack ::
  UTCTime ->
  StdGen ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  Sem TestStack a ->
  IO (Either MeetingError a)
runTestStack now gen teams configs =
  runM
    . fmap (either (error . show) (either (error . show) Imports.id))
    . runError @(Tagged 'TeamNotFound ())
    . runError @(Tagged 'TeamMemberNotFound ())
    . evalState gen
    . randomToStatefulStdGen
    . runInputConst (toLocalUnsafe (Domain "my-domain") ())
    . evalState now
    . interpretNowAsState
    . miniGalleyAPIAccess teams configs
    . evalState ([] :: [Push])
    . evalState Map.empty
    . evalState Map.empty
    . evalState Map.empty
    . runError @MeetingError
    . discardTinyLogs
    . inMemoryNotificationSubsystemInterpreter
    . interpretFeaturesConfigSubsystemPure configs
    . interpretTeamSubsystemToGalleyAPI
    . inMemoryConversationSubsystemInterpreter
    . inMemoryMeetingsStoreInterpreter
    . interpretMeetingNotifier
    . interpretMeetingsSubsystem
      MeetingSystemConfig
        { legacyTimeZone = API.defaultLegacyTimeZone,
          validityPeriod = 3600,
          pastEditPeriod = configuredPastEditPeriod
        }

-- | Decode all 'Push' payloads that are meeting lifecycle events. Any push that
-- decodes as a 'MeetingEvent.Event' is one: conversation events use distinct
-- @type@ tags that the meeting 'EventType' enum rejects.
extractMeetingEvents :: [Push] -> [MeetingEvent.Event]
extractMeetingEvents pushes =
  [e | push <- pushes, Success e <- [fromJSON (Object push.json)]]

spec :: Spec
spec = describe "MeetingsSubsystem.Interpreter" $ do
  it "creates a meeting and can retrieve it" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      meeting <- createMeeting zUser (ConnId "test-conn") newMeeting
      fetched <- getMeeting zUser meeting.meeting.id
      pure (meeting, fetched)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (meeting, fetched) -> do
        meeting.meeting.title `shouldBe` fromJust (checked "Test Meeting")
        meeting.conversation.qualifiedId `shouldBe` meeting.meeting.conversationId
        fetched `shouldBe` Just meeting.meeting

  it "creates meeting conversation with invite access for MLS participant adds" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Access Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      meeting <- createMeeting zUser (ConnId "test-conn") newMeeting
      pure meeting.conversation.metadata.cnvmAccess

    case result of
      Left err -> fail $ "Error: " <> show err
      Right access -> do
        PrivateAccess `elem` access `shouldBe` True
        InviteAccess `elem` access `shouldBe` True

  it "fails to create a meeting if end_time is not after start_time" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Invalid Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 3500 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (ConnId "test-conn") newMeeting
    result `shouldBe` Left (InvalidTimes EndBeforeStart)

  it "fails to create a meeting if start time is in the past" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Past Meeting",
              startTime = addUTCTime (negate 3600) now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (ConnId "test-conn") newMeeting
    result `shouldBe` Left (InvalidTimes StartTimeTooFarInPast)

  it "accepts a meeting whose start time is exactly at the tolerance boundary" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        -- Exactly `expectedStartTimeTolerance` in the past: the check is strict (`<`),
        -- so the boundary itself is still accepted.
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Boundary Meeting",
              startTime = addUTCTime (negate expectedStartTimeTolerance) now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (ConnId "test-conn") newMeeting
    result `shouldSatisfy` isRight

  it "rejects a meeting whose start time is just past the tolerance boundary" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        -- One second past the tolerance boundary.
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Just Past Boundary Meeting",
              startTime = addUTCTime (negate (expectedStartTimeTolerance + 1)) now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (ConnId "test-conn") newMeeting
    result `shouldBe` Left (InvalidTimes StartTimeTooFarInPast)

  describe "getMeeting access control" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        uid3 = Id $ read "00000000-0000-0000-0000-000000000003"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        zUser3 = toLocalUnsafe (Domain "wire.com") uid3
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig = npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def

    it "returns Nothing for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Past Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        passTime validityWindow
        getMeeting zUser1 meeting.meeting.id

      result `shouldBe` Right Nothing

    it "returns meeting for creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Creator Access Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        (meeting,) <$> getMeeting zUser1 meeting.meeting.id

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, Just m) -> m.id `shouldBe` meeting.meeting.id
        Right (_, Nothing) -> fail "Expected Just meeting for creator"

    it "returns meeting for conversation member" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Member Access Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        members <- gets (Map.lookup (qUnqualified meeting.conversation.qualifiedId))
        let updatedMembers = maybe (Set.singleton uid2) (Set.insert uid2) members
        modify (Map.insert (qUnqualified meeting.conversation.qualifiedId) updatedMembers)
        (meeting,) <$> getMeeting zUser2 meeting.meeting.id

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, Just m) -> m.id `shouldBe` meeting.meeting.id
        Right (_, Nothing) -> fail "Expected Just meeting for conversation member"

    it "returns Nothing for unauthorized user" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Unauthorized Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        getMeeting zUser3 meeting.meeting.id

      result `shouldBe` Right Nothing

  describe "updateMeeting" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def

    it "throws EmptyUpdate when no fields provided" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing Nothing Nothing)

      result `shouldBe` Left EmptyUpdate

    it "throws InvalidTimes when end_time is not after start_time" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Nothing,
                  endTime = Just (addUTCTime 3600 now),
                  title = Nothing,
                  recurrence = Nothing
                }
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes EndBeforeStart)

    it "allows editing an upcoming meeting's start time into the past within the past-edit window" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate 60) now),
                  endTime = Nothing,
                  title = Nothing,
                  recurrence = Nothing
                }
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update

      case result of
        Left err -> fail $ "Expected the update to be applied, got: " <> show err
        Right Nothing -> fail "Expected the update to be applied"
        Right (Just updated) ->
          updated.meeting.startTime `shouldBe` addUTCTime (negate 60) now

    it "accepts a meeting update whose start time is exactly at the past-edit boundary" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate configuredPastEditPeriod) now),
                  endTime = Nothing,
                  title = Nothing,
                  recurrence = Nothing
                }
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update

      case result of
        Right (Just _) -> pure ()
        other -> fail $ "Expected the update to be applied, got: " <> show other

    it "rejects a meeting update whose start time is just past the past-edit boundary" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate (configuredPastEditPeriod + 1)) now),
                  endTime = Nothing,
                  title = Nothing,
                  recurrence = Nothing
                }
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes TimesBeyondPastEditWindow)

    it "throws InvalidTimes when updating both times beyond the past-edit window even when start < end" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate 7200) now),
                  endTime = Just (addUTCTime (negate (configuredPastEditPeriod + 1)) now),
                  title = Nothing,
                  recurrence = Nothing
                }
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes TimesBeyondPastEditWindow)

    it "allows editing an already-started (ongoing) meeting, including its start time" $ do
      let ongoingMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Ongoing Meeting",
                startTime = addUTCTime 100 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") ongoingMeeting
          -- Advance the clock 3000s: startTime (now+100s) is now in the past, so
          -- the meeting has started. It stays editable because isAlive is
          -- effective-end-based (end_time = now+7200s), still past
          -- the alive-cutoff (now+3000s-3600s = now-600s).
          passTime 3000
          let update =
                API.UpdateMeeting
                  { -- Original start time, which is now in the past.
                    startTime = Just (addUTCTime 100 now),
                    endTime = Nothing,
                    title = Just (unsafeRange "Edited While Ongoing"),
                    recurrence = Nothing
                  }
          updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update
      case result of
        Left err ->
          fail $ "Expected the ongoing meeting to be editable, got: " <> show err
        Right Nothing -> fail "Expected the update to be applied"
        Right (Just updated) ->
          updated.meeting.title `shouldBe` unsafeRange "Edited While Ongoing"

    it "still throws InvalidTimes when editing an ongoing meeting to startTime >= endTime" $ do
      let ongoingMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Ongoing Meeting",
                startTime = addUTCTime 100 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") ongoingMeeting
          passTime 3000
          let update =
                API.UpdateMeeting
                  { startTime = Just (addUTCTime 8000 now),
                    endTime = Nothing,
                    title = Nothing,
                    recurrence = Nothing
                  }
          updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes EndBeforeStart)

    it "returns Nothing for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        passTime validityWindow
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Test")) Nothing)

      result `shouldBe` Right Nothing

    it "returns Nothing for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Update",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        updateMeeting zUser2 (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Test")) Nothing)

      result `shouldBe` Right Nothing

    it "returns Nothing when the meeting's conversation is missing" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Orphaned Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        -- Simulate a data-inconsistency: the meeting's conversation vanished.
        modify @(Map ConvId StoredConversation) (Map.delete (qUnqualified meeting.meeting.conversationId))
        updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing)

      result `shouldBe` Right Nothing

    prop "applies valid update, preserves unchanged fields" $ \(update :: API.UpdateMeeting) ->
      let baseMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Title",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }
          -- Clamp the updated start/end times so they are not in the past
          -- beyond the past-edit window. This avoids discarding
          -- QuickCheck-generated updates whose arbitrary UTCTime is far from
          -- `now`.
          sanitizedUpdate =
            API.UpdateMeeting
              (fmap (max (addUTCTime (negate 60) now)) update.startTime)
              (fmap (max (addUTCTime (negate 60) now)) update.endTime)
              update.title
              update.recurrence
          effectiveStart = fromMaybe baseMeeting.startTime sanitizedUpdate.startTime
          effectiveEndTime = fromMaybe baseMeeting.endTime sanitizedUpdate.endTime
          isNotEmpty = sanitizedUpdate /= API.UpdateMeeting Nothing Nothing Nothing Nothing
          hasValidTimes = effectiveEndTime > effectiveStart
       in isNotEmpty && hasValidTimes ==>
            ioProperty $ do
              result <- runTestStack now gen Map.empty teamConfig $ do
                meeting <- createMeeting zUser1 (ConnId "test-conn") baseMeeting
                updated <- updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id sanitizedUpdate
                pure (meeting.meeting.conversationId, updated)
              case result of
                Left err ->
                  pure $ counterexample ("Unexpected error: " <> show err) False
                Right (_, Nothing) ->
                  pure $ counterexample "Expected Just meeting, got Nothing" False
                Right (convId, Just m) ->
                  pure $
                    m.meeting.title === fromMaybe baseMeeting.title sanitizedUpdate.title
                      .&&. m.meeting.startTime === effectiveStart
                      .&&. m.meeting.endTime === effectiveEndTime
                      .&&. m.meeting.recurrence === fromMaybe baseMeeting.recurrence sanitizedUpdate.recurrence
                      .&&. m.meeting.conversationId === convId

  describe "deleteMeeting" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        testConnId = ConnId (C.pack "test-conn")

    it "returns True for successful deletion by creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        deleteResult <- deleteMeeting zUser1 testConnId meeting.meeting.id
        getResult <- getMeeting zUser1 meeting.meeting.id
        pure (deleteResult, getResult)

      result `shouldBe` Right (True, Nothing)

    it "returns False when non-creator tries to delete" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        deleteMeeting zUser2 testConnId meeting.meeting.id

      result `shouldBe` Right False

    it "returns False for expired meeting deletion" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        passTime validityWindow
        deleteMeeting zUser1 testConnId meeting.meeting.id

      result `shouldBe` Right False

    it "returns False when meeting does not exist" $ do
      let meetingId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <- runTestStack now gen Map.empty teamConfig $ do
        deleteMeeting zUser1 testConnId meetingId

      result `shouldBe` Right False

    it "deletes associated meeting conversation" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        _ <- internalGetConversation (qUnqualified meeting.conversation.qualifiedId)
        _ <- deleteMeeting zUser1 testConnId meeting.meeting.id
        internalGetConversation (qUnqualified meeting.conversation.qualifiedId)

      result `shouldBe` Right Nothing

    it "preserves non-meeting conversation" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        -- Change conversation type to non-meeting by updating local members only
        -- This simulates a non-meeting conversation without touching internal types
        deleteMeeting zUser1 testConnId meeting.meeting.id

      result `shouldSatisfy` isRight

  describe "addInvitedEmails" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        email1 = unsafeEmailAddress "user1" "example.com"
        email2 = unsafeEmailAddress "user2" "example.com"

    it "returns True and adds emails for creator of valid meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- addInvitedEmails zUser1 meeting.meeting.id [email1, email2]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email1, email2]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns False for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        passTime validityWindow
        addInvitedEmails zUser1 meeting.meeting.id [email1]

      result `shouldBe` Right False

    it "returns False for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Test",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        addInvitedEmails zUser2 meeting.meeting.id [email1]

      result `shouldBe` Right False

    it "returns False for non-existent meeting" $ do
      let nonExistentId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <-
        runTestStack now gen Map.empty teamConfig $
          addInvitedEmails zUser1 nonExistentId [email1]

      result `shouldBe` Right False

  describe "removeInvitedEmails" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        email1 = unsafeEmailAddress "user1" "example.com"
        email2 = unsafeEmailAddress "user2" "example.com"
        email3 = unsafeEmailAddress "user3" "example.com"

    it "returns True and removes emails for creator of valid meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2, email3]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- removeInvitedEmails zUser1 meeting.meeting.id [email2]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email1, email3]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns True when removing all emails" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- removeInvitedEmails zUser1 meeting.meeting.id [email1, email2]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` []
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns True when removing non-existent emails" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- removeInvitedEmails zUser1 meeting.meeting.id [email2, email3]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email1]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns False for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        passTime validityWindow
        removeInvitedEmails zUser1 meeting.meeting.id [email1]

      result `shouldBe` Right False

    it "returns False for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Test",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        removeInvitedEmails zUser2 meeting.meeting.id [email1]

      result `shouldBe` Right False

    it "returns False for non-existent meeting" $ do
      let nonExistentId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <-
        runTestStack now gen Map.empty teamConfig $
          removeInvitedEmails zUser1 nonExistentId [email1]

      result `shouldBe` Right False

  describe "replaceInvitedEmails" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        email1 = unsafeEmailAddress "user1" "example.com"
        email2 = unsafeEmailAddress "user2" "example.com"
        email3 = unsafeEmailAddress "user3" "example.com"

    it "returns True and replaces emails for creator of valid meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- replaceInvitedEmails zUser1 meeting.meeting.id [email3]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email3]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns True when replacing with an empty list" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- replaceInvitedEmails zUser1 meeting.meeting.id []
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` []
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "deduplicates emails when replacing (mirrors Postgres semantics)" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        success <- replaceInvitedEmails zUser1 meeting.meeting.id [email3, email3, email1]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email3, email1]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns False for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        passTime validityWindow
        replaceInvitedEmails zUser1 meeting.meeting.id [email2]

      result `shouldBe` Right False

    it "returns False for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Test",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
        replaceInvitedEmails zUser2 meeting.meeting.id [email3]

      result `shouldBe` Right False

    it "returns False for non-existent meeting" $ do
      let nonExistentId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <-
        runTestStack now gen Map.empty teamConfig $
          replaceInvitedEmails zUser1 nonExistentId [email1]

      result `shouldBe` Right False

  describe "recurrence vs expiry" $ do
    -- validityPeriod in runTestStack is 3600s, so a meeting whose endTime is
    -- more than 1h in the past is treated as expired unless its recurrence
    -- window keeps it alive.
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $
            def
        -- Meetings are created with future times. We then advance the mock
        -- clock past the validity window (11000s) so that the original slot
        -- has passed, while the recurrence window stays open.
        futureMeeting r =
          API.NewMeeting
            { title = fromJust $ checked "Recurring Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = r,
              invitedEmails = []
            }
        boundedRecurrence =
          Just $
            API.Recurrence
              { freq = API.Daily,
                interval = 1,
                until = Just (addUTCTime (30 * nominalDay) now)
              }
        openEndedRecurrence =
          Just $
            API.Recurrence
              { freq = API.Daily,
                interval = 1,
                until = Nothing
              }
        meetingAt endOffset r =
          API.NewMeeting
            { title = fromJust $ checked "Meeting",
              startTime = addUTCTime (endOffset - 3600) now,
              endTime = addUTCTime endOffset now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = r,
              invitedEmails = []
            }
        recurUntil t =
          Just (API.Recurrence {freq = API.Daily, interval = 1, until = Just t})

    it "getMeeting returns a recurring meeting whose slot passed but window is open" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          getMeeting zUser meeting.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right Nothing -> fail "Expected Just meeting (recurrence window still open)"
        Right (Just _) -> pure ()

    it "listMeetings includes a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          _meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          listMeetings zUser
      case result of
        Left err -> fail $ "Error: " <> show err
        Right xs -> length xs `shouldBe` 1

    it "updateMeeting succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          updateMeeting zUser (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing)
      fmap isJust result `shouldBe` Right True

    it "addInvitedEmails succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          addInvitedEmails zUser meeting.meeting.id [unsafeEmailAddress "user" "example.com"]
      result `shouldBe` Right True

    it "deleteMeeting succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          deleteMeeting zUser (ConnId "test-conv") meeting.meeting.id
      result `shouldBe` Right True

    it "removeInvitedEmails succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          removeInvitedEmails zUser meeting.meeting.id [unsafeEmailAddress "user" "example.com"]
      result `shouldBe` Right True

    it "replaceInvitedEmails succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          passTime validityWindow
          replaceInvitedEmails zUser meeting.meeting.id [unsafeEmailAddress "user" "example.com"]
      result `shouldBe` Right True

    it "getMeeting returns an open-ended recurring meeting indefinitely" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting openEndedRecurrence)
          passTime validityWindow
          getMeeting zUser meeting.meeting.id
      fmap isJust result `shouldBe` Right True

    it "cleanupOldMeetings skips recurring meetings whose window is still open" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          recurring <- createMeeting zUser (ConnId "test-conn") (futureMeeting boundedRecurrence)
          _plain <- createMeeting zUser (ConnId "test-conn") (futureMeeting Nothing)
          passTime validityWindow
          -- cutoff is past the endTime (now+7200) so the non-recurring
          -- meeting is picked up, but well before the recurrence window.
          deleted <- cleanupOldMeetings (addUTCTime 7300 now) 100
          remaining <- getMeeting zUser recurring.meeting.id
          pure (deleted, fmap (.id) remaining, recurring.meeting.id)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (deleted, remainingId, recurringId) -> do
          -- only the non-recurring meeting is past the cleanup cutoff
          deleted `shouldBe` 1
          remainingId `shouldBe` Just recurringId

    it "cleanupOldMeetings never picks up open-ended recurring meetings" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (ConnId "test-conn") (futureMeeting openEndedRecurrence)
          passTime validityWindow
          -- Even with a cutoff well past the endTime, open-ended
          -- recurrence is never picked up.
          deleted <- cleanupOldMeetings (addUTCTime validityWindow now) 100
          remaining <- getMeeting zUser meeting.meeting.id
          pure (deleted, fmap (.id) remaining, meeting.meeting.id)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (deleted, remainingId, meetingId) -> do
          deleted `shouldBe` 0
          remainingId `shouldBe` Just meetingId

    it "cleanupOldMeetings deletes in effectiveEndTime order, not endTime order" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          -- endTime now+8000, no recurrence -> effectiveEndTime now+8000 (earliest)
          plain <- createMeeting zUser (ConnId "test-conn") (meetingAt 8000 Nothing)
          -- endTime now+4000, until now+10000 -> effectiveEndTime now+10000 (later)
          recur <- createMeeting zUser (ConnId "test-conn") (meetingAt 4000 (recurUntil (addUTCTime 10000 now)))
          _deleted <- cleanupOldMeetings (addUTCTime 11000 now) 1
          plainRemains <- isJust <$> getMeeting zUser plain.meeting.id
          recurRemains <- isJust <$> getMeeting zUser recur.meeting.id
          pure (plainRemains, recurRemains)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (plainRemains, recurRemains) -> do
          -- plain has the earlier effectiveEndTime, so it is deleted first;
          -- recur survives. With the old endTime sort, recur (endTime now+4000)
          -- would be deleted first instead.
          plainRemains `shouldBe` False
          recurRemains `shouldBe` True

    prop "aliveness follows effectiveEndTime across get/list/cleanup" $
      \(recurrence :: Maybe API.Recurrence) (advance :: NonNegative Int) ->
        let startTime = addUTCTime 3600 now
            endTime = addUTCTime 7200 now
            nm =
              API.NewMeeting
                { title = fromJust $ checked "Recurring Meeting",
                  startTime = startTime,
                  endTime = addUTCTime 3600 startTime,
                  tzid = API.defaultLegacyTimeZone,
                  recurrence = recurrence,
                  invitedEmails = []
                }
            advanceTime = fromIntegral (getNonNegative advance) :: NominalDiffTime
            -- After advancing the clock, the validity cutoff moves forward.
            cutoff = addUTCTime (advanceTime - 3600) now
            effEnd = maybe (Just endTime) (\r -> max endTime <$> r.until) recurrence
            alive = maybe True (>= cutoff) effEnd
         in ioProperty $ do
              result <-
                runTestStack now gen Map.empty teamConfig $ do
                  meeting <- createMeeting zUser (ConnId "test-conn") nm
                  passTime advanceTime
                  fetched <- isJust <$> getMeeting zUser meeting.meeting.id
                  listedCount <- length <$> listMeetings zUser
                  deleted <- cleanupOldMeetings cutoff 100
                  remains <- isJust <$> getMeeting zUser meeting.meeting.id
                  pure (fetched, listedCount, deleted, remains)
              pure $ case result of
                Left err -> counterexample ("Unexpected error: " <> show err) False
                Right (fetched, listedCount, deleted, remains) ->
                  (fetched === alive)
                    .&&. (listedCount === if alive then 1 else 0)
                    .&&. (deleted === if alive then 0 else 1)
                    .&&. (remains === alive)

  describe "checkMeetingsEnabled" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uidPersonal = Id $ read "00000000-0000-0000-0000-000000000001"
        uidTeam = Id $ read "00000000-0000-0000-0000-000000000002"
        zUserPersonal = toLocalUnsafe (Domain "wire.com") uidPersonal
        zUserTeam = toLocalUnsafe (Domain "wire.com") uidTeam
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember = mkTeamMember uidTeam fullPermissions Nothing UserLegalHoldDisabled
        meetingsEnabled =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def
        meetingsDisabled =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusDisabled LockStatusUnlocked def) def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    it "allows operations for personal user even when meetings disabled" $ do
      result <-
        runTestStack now gen Map.empty meetingsDisabled $
          createMeeting zUserPersonal (ConnId "test-conn") newMeeting

      result `shouldSatisfy` isRight

    it "allows operations for team user with meetings enabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      result `shouldSatisfy` isRight

    it "throws MeetingsFeatureDisabled on createMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      result `shouldBe` Left MeetingsFeatureDisabled

    it "returns Nothing on getMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              getMeeting zUserTeam meeting.meeting.id

          result2 `shouldBe` Right Nothing

    it "throws MeetingsFeatureDisabled on updateMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              updateMeeting zUserTeam (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing)

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "throws MeetingsFeatureDisabled on deleteMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              deleteMeeting zUserTeam (ConnId "test-conn") meeting.meeting.id

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "returns [] on listMeetings for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
          listMeetings zUserTeam

      result `shouldBe` Right []

    it "throws MeetingsFeatureDisabled on addInvitedEmails for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              addInvitedEmails zUserTeam meeting.meeting.id [unsafeEmailAddress "test" "example.com"]

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "throws MeetingsFeatureDisabled on removeInvitedEmails for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              removeInvitedEmails zUserTeam meeting.meeting.id [unsafeEmailAddress "test" "example.com"]

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "throws MeetingsFeatureDisabled on replaceInvitedEmails for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (ConnId "test-conn") newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              replaceInvitedEmails zUserTeam meeting.meeting.id [unsafeEmailAddress "test" "example.com"]

          result2 `shouldBe` Left MeetingsFeatureDisabled

  describe "meeting events" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Event Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    it "emits a meeting.create event on successful create" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
          pushes <- get @[Push]
          pure (meeting, pushes)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, pushes) -> do
          let events = extractMeetingEvents pushes
          length events `shouldBe` 1
          (head events).evtType `shouldBe` MeetingEvent.Create
          (head events).evtMeeting `shouldBe` meeting.meeting.id

    it "emits a meeting.update event on successful update" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
          put @[Push] []
          _ <- updateMeeting zUser1 (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing)
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> do
          let events = extractMeetingEvents pushes
          length events `shouldBe` 1
          (head events).evtType `shouldBe` MeetingEvent.Update

    it "emits a meeting.delete event on successful delete" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
          put @[Push] []
          _ <- deleteMeeting zUser1 (ConnId "test-conn") meeting.meeting.id
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> do
          let events = extractMeetingEvents pushes
          length events `shouldBe` 1
          (head events).evtType `shouldBe` MeetingEvent.Delete

    it "does not emit an event when updateMeeting fails (non-creator)" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
          put @[Push] []
          _ <- updateMeeting zUser2 (ConnId "test-conn") meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Hijack")) Nothing)
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> extractMeetingEvents pushes `shouldBe` []

    it "does not emit an event when deleteMeeting fails (non-creator)" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
          meeting <- createMeeting zUser1 (ConnId "test-conn") newMeeting
          put @[Push] []
          _ <- deleteMeeting zUser2 (ConnId "test-conn") meeting.meeting.id
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> extractMeetingEvents pushes `shouldBe` []

    it "delivers lifecycle events to all members and propagates conn" $ do
      let members =
            [ newMember uid1,
              newMember uid2
            ]
          meetingId = Id $ read "00000000-0000-0000-0000-000000000070"
          convId = Id $ read "00000000-0000-0000-0000-000000000071"
          qMeetingId = Qualified meetingId (Domain "wire.com")
          qConvId = Qualified convId (Domain "wire.com")
          originConn = ConnId "origin"
      result <-
        runTestStack now gen Map.empty def $ do
          _ <- notifyMeetingEvent zUser1 (Just originConn) members qConvId Nothing MeetingEvent.Create qMeetingId
          get @[Push]
      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> do
          let push = head pushes
          -- Order is deterministic: 'members' is a literal list, 'map
          -- localMemberToRecipient' preserves order, and mkMeetingEventPush
          -- no longer filters or reorders recipients.
          map (.recipientUserId) push.recipients `shouldBe` [uid1, uid2]
          push.conn `shouldBe` Just originConn

  describe "V16 operations" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        startT = addUTCTime 3600 now
        endT = addUTCTime 7200 now
        newLegacy =
          API.NewMeetingV16
            { startTime = startT,
              endTime = endT,
              recurrence = Nothing,
              title = fromJust $ checked "Legacy Meeting",
              invitedEmails = []
            }

    it "createMeetingV16 then getMeetingV16 returns the requested end_time" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV16 zUser (ConnId "test-conn") newLegacy
          getMeetingV16 zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just lm) -> lm.endTime `shouldBe` endT
        Right Nothing -> fail "expected a meeting"

    it "a meeting created via the new shape is visible to legacy clients with the equivalent end_time" $ do
      let nm =
            API.NewMeeting
              { title = fromJust $ checked "New Meeting",
                startTime = startT,
                endTime = addUTCTime 3600 startT,
                tzid = API.defaultLegacyTimeZone,
                recurrence = Nothing,
                invitedEmails = []
              }
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeeting zUser (ConnId "test-conn") nm
          getMeetingV16 zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just lm) -> lm.endTime `shouldBe` endT
        Right Nothing -> fail "expected a meeting"

    it "a meeting created via the legacy shape is visible to new clients with end_time and the default tzid" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV16 zUser (ConnId "test-conn") newLegacy
          getMeeting zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just m) -> do
          m.endTime `shouldBe` endT
          m.tzid `shouldBe` API.defaultLegacyTimeZone
        Right Nothing -> fail "expected a meeting"

    it "updateMeetingV16 moving both start and end keeps the requested end_time" $ do
      let newStart = addUTCTime 8000 now
          newEnd = addUTCTime 9000 now
          upd =
            API.UpdateMeeting
              { startTime = Just newStart,
                endTime = Just newEnd,
                title = Nothing,
                recurrence = Nothing
              }
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV16 zUser (ConnId "test-conn") newLegacy
          updateMeetingV16 zUser (ConnId "test-conn") mwc.meeting.id upd
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just mwc') -> mwc'.meeting.endTime `shouldBe` newEnd
        Right Nothing -> fail "expected the update to apply"

  describe "StoredMeeting postgres unmarshall" $ do
    -- A backfilled row (tzid NOT NULL, end_time set): end_time is the truth
    -- and tzid is read straight off the column.
    it "parses end_time and tzid for a backfilled row" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          t1 = addUTCTime 3600 t0
          legacyRow :: Store.StoredMeetingTuple
          legacyRow =
            ( nil,
              "t",
              nil,
              t0,
              t1,
              "Europe/Berlin",
              Nothing,
              Nothing,
              Nothing,
              nil,
              V.empty,
              False,
              t0,
              t0
            )
      let result :: Either Text Store.StoredMeeting
          result = postgresUnmarshall legacyRow
      case result of
        Left e -> expectationFailure $ "unmarshall failed: " <> show e
        Right sm -> do
          sm.endTime `shouldBe` t1
          sm.tzid `shouldBe` API.defaultLegacyTimeZone

    it "rejects a non-null but unparseable tzid instead of swallowing it" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          t1 = addUTCTime 3600 t0
          badTzRow :: Store.StoredMeetingTuple
          badTzRow =
            ( nil,
              "t",
              nil,
              t0,
              t1,
              "not-a-zone",
              Nothing,
              Nothing,
              Nothing,
              nil,
              V.empty,
              False,
              t0,
              t0
            )
      let result :: Either Text Store.StoredMeeting
          result = postgresUnmarshall badTzRow
      case result of
        Left msg -> msg `shouldBe` ("invalid tzid" :: Text)
        Right _ -> expectationFailure "expected unmarshall to reject an invalid tzid"

-- | Synchronize with 'Wire.MeetingsSubsystem.Interpreter.startTimeTolerance'
expectedStartTimeTolerance :: NominalDiffTime
expectedStartTimeTolerance = 60

-- | Past-edit window configured in 'runTestStack'.
configuredPastEditPeriod :: NominalDiffTime
configuredPastEditPeriod = 3600

-- | Validity window, beyond this one-time meeting belong to the past
validityWindow :: NominalDiffTime
validityWindow = 11000
